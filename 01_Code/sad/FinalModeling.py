"""Execute chronological *batch order*, not a claim-time experiment.

All splits, acquisition masks, prediction-level outputs and convergence
warnings are retained for audit. No outcome is used to form a shortlist.
"""
from .common import *
from .PreProcessing import *
from .EntropyMethod import entropy_gain_scores
from .Labeling import anomaly_votes, shortlist_initial, reviewed_indices, top_fraction
from .Modeling import fit_models, predict_ensemble
from .Evaluation import metrics_row
from .Comparators import tune_pso, fit_comparators
from dataclasses import replace

ARCH_NAMES = {"simple_a":"Simple-A", "simple_b":"Simple-B",
              "multilayer":"Multilayer"}

def prepare(df, target, cfg, seed):
    y = to_binary_target(df[target])
    raw = df.drop(columns=[target])
    parts = stratified_partitions(y, seed, cfg.partition_fractions)
    assert len(set(np.concatenate(parts))) == len(y)
    xp = [raw.iloc[idx].reset_index(drop=True) for idx in parts]
    plan = FeaturePlan(cfg).fit(xp[0])
    xp = [plan.transform(x) for x in xp]
    encoder = make_encoder(plan).fit(xp[0])
    z = [np.asarray(encoder.transform(x), dtype=float) for x in xp]
    assert all(np.isfinite(x).all() for x in z)
    start = time.perf_counter()
    votes, entropy = anomaly_votes(xp[0], plan, z[0], cfg, seed)
    return dict(y=y, parts=parts, xp=xp, z=z, plan=plan, encoder=encoder,
                votes=votes, entropy=entropy,
                ad_seconds=time.perf_counter()-start)

def initial(prep, arch, cfg):
    if arch != "multilayer":
        return shortlist_initial(prep["votes"], prep["entropy"], arch, cfg)
    count = prep["votes"].sum(axis=1).to_numpy()
    candidate = np.flatnonzero(count >= cfg.tad)
    hit = np.zeros(len(count), dtype=bool)
    if len(candidate):
        # Crucial correction: gain AND bins use only the cascade candidate set.
        local_x = prep["xp"][0].iloc[candidate]
        gain = entropy_gain_scores(local_x, prep["plan"].numeric_, prep["plan"].categorical_)
        hit[candidate[top_fraction(gain, cfg.entropy_tail_fraction).astype(bool)]] = True
    return hit, count / 9 * (.5 + .5 * hit)

def trajectory(prep, dataset, seed, arch, cfg, setting, out):
    name = ARCH_NAMES[arch]
    pred_frames, metric_rows, timing, warning_rows, tuning = [], [], [], [], []
    train_records = []; model_cache = {}
    yy = [prep["y"][p] for p in prep["parts"]]
    fit_counter = 0
    def timed_fit(train_x, train_y, model_seed, stage):
        nonlocal fit_counter
        start = time.perf_counter()
        with warnings.catch_warnings(record=True) as recorded:
            warnings.simplefilter("always")
            fitted = fit_models(train_x, train_y, model_seed)
        for w in recorded:
            warning_rows.append(dict(stage=stage, category=w.category.__name__, message=str(w.message)))
        timing.append(dict(stage=stage, operation="fit_8_models",
                           seconds=time.perf_counter()-start, verified_n=len(train_y),
                           verified_fraud_n=int(train_y.sum())))
        fit_counter += 1
        return fitted

    def record(j, variant, hit, score, votes_count, is_fit_source=False):
        # Mask fixed before any target access.
        rng = np.random.default_rng(seed + 10000 + j)
        selected = reviewed_indices(hit.astype(bool), cfg.normal_audit_fraction, rng)
        reviewed = np.zeros(len(hit), dtype=bool); reviewed[selected] = True
        frame = pd.DataFrame(dict(row_id=prep["parts"][j] + 1, partition=f"D{j+1}",
            model=variant, y=yy[j], pred=hit.astype(int), score=score,
            votes=votes_count, reviewed=reviewed.astype(int),
            audited=(reviewed & ~hit.astype(bool)).astype(int)))
        pred_frames.append(frame)
        row = metrics_row(yy[j], hit, score, dataset=dataset, seed=seed,
                          setting=setting, architecture=variant, partition=f"D{j+1}")
        row.update(reviewed_n=int(reviewed.sum()), review_rate=float(reviewed.mean()))
        metric_rows.append(row)
        if is_fit_source:
            train_records.extend(prep["parts"][j][selected].tolist())
        return selected

    hit, score = initial(prep, arch, cfg)
    selected = record(0, name, hit.astype(int), score, prep["votes"].sum(axis=1).to_numpy(), True)
    train_x = prep["z"][0][selected]; train_y = yy[0][selected]
    models = timed_fit(train_x, train_y, seed, "D1")
    frozen = models
    extra = None; pso_params = None
    if arch == "simple_b" and setting == "Main":
        start = time.perf_counter()
        pso_params, tuning = tune_pso(train_x, train_y, seed)
        extra = fit_comparators(train_x, train_y, seed, pso_params)
        timing.append(dict(stage="D1", operation="tune_and_fit_2_comparators",
                           seconds=time.perf_counter()-start, verified_n=len(train_y),
                           verified_fraud_n=int(train_y.sum())))
    for j in range(1, 4):
        # Disjointness gate against every record used to fit the current models.
        assert not set(train_records).intersection(prep["parts"][j])
        start = time.perf_counter()
        pred, prob, hard, probs = predict_ensemble(models, prep["z"][j], cfg.tml)
        timing.append(dict(stage=f"D{j+1}", operation="predict_8_models",
                           seconds=time.perf_counter()-start, verified_n=len(train_y),
                           verified_fraud_n=int(train_y.sum())))
        chosen = record(j, name, pred, prob, hard.sum(axis=1), j < 3)
        if arch == "multilayer" and setting == "Main":
            sp, ss, sh, _ = predict_ensemble(frozen, prep["z"][j], cfg.tml)
            record(j, "Multilayer-static", sp, ss, sh.sum(axis=1))
        if arch == "simple_b" and setting == "Main":
            for k, model_name in enumerate(models):
                record(j, model_name, hard[:, k], probs[:, k], hard[:, k])
            for model_name, model in extra.items():
                s = model.predict_proba(prep["z"][j])[:, 1]; p = (s >= .5).astype(int)
                record(j, model_name, p, s, p)
        if j < 3:
            train_x = np.vstack([train_x, prep["z"][j][chosen]])
            train_y = np.concatenate([train_y, yy[j][chosen]])
            models = timed_fit(train_x, train_y, seed+j, f"D{j+1}")
            if extra is not None:
                start = time.perf_counter()
                extra = fit_comparators(train_x, train_y, seed+j, pso_params)
                timing.append(dict(stage=f"D{j+1}", operation="fit_2_comparators",
                    seconds=time.perf_counter()-start, verified_n=len(train_y),
                    verified_fraud_n=int(train_y.sum())))
    frames = pd.concat(pred_frames, ignore_index=True)
    for variant, q in frames[frames.partition.ne("D1")].groupby("model", sort=False):
        row = metrics_row(q.y.to_numpy(), q.pred.to_numpy(), q.score.to_numpy(),
            dataset=dataset, seed=seed, setting=setting, architecture=variant, partition="D2-D4")
        row.update(reviewed_n=int(q.reviewed.sum()), review_rate=float(q.reviewed.mean()))
        metric_rows.append(row)
    # Include exact keys in prediction files so concatenation preserves provenance.
    frames.insert(0, "setting", setting); frames.insert(0, "seed", seed)
    frames.insert(0, "dataset", dataset)
    out.mkdir(parents=True, exist_ok=True)
    frames.to_csv(out / "predictions.csv.gz", index=False, compression="gzip", float_format="%.16g")
    pd.DataFrame(metric_rows).to_csv(out / "metrics.csv", index=False)
    for rows, filename in [(timing, "timing.json"), (warning_rows, "warnings.json"), (tuning, "pso_trace.json")]:
        (out / filename).write_text(json.dumps(rows, indent=2), encoding="utf-8")
    return metric_rows
