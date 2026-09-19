from .common import *
from .PreProcessing import *
from .EntropyMethod import entropy_gain_scores


def union_univariate_flags(a: np.ndarray, rule) -> np.ndarray:
    flag = np.zeros(a.shape[0], dtype=bool)
    for j in range(a.shape[1]):
        flag |= rule(a[:, j])
    return flag.astype(int)


def grubbs_flags(a: np.ndarray, alpha: float = 0.05) -> np.ndarray:
    n = a.shape[0]
    out = np.zeros(n, dtype=bool)
    if n < 3:
        return out.astype(int)
    tcrit = t.ppf(1 - alpha / (2 * n), n - 2)
    gcrit = ((n - 1) / math.sqrt(n)) * math.sqrt(tcrit**2 / (n - 2 + tcrit**2))
    for j in range(a.shape[1]):
        col = a[:, j]
        sd = np.std(col, ddof=1)
        if not np.isfinite(sd) or sd == 0:
            continue
        mean = np.mean(col)
        lo, hi = int(np.argmin(col)), int(np.argmax(col))
        if abs(col[lo] - mean) / sd > gcrit:
            out[lo] = True
        if abs(col[hi] - mean) / sd > gcrit:
            out[hi] = True
    return out.astype(int)


def top_fraction(scores: np.ndarray, fraction: float) -> np.ndarray:
    n = len(scores)
    k = max(1, int(math.ceil(fraction * n)))
    idx = np.argpartition(np.asarray(scores), n - k)[n - k :]
    out = np.zeros(n, dtype=int)
    out[idx] = 1
    return out


def anomaly_votes(x_first: pd.DataFrame, plan: FeaturePlan, encoded_first: np.ndarray, cfg: SADConfig, seed: int):
    num = numeric_matrix(x_first, plan.numeric_)
    cat = categorical_matrix(x_first, plan.categorical_)
    n = len(x_first)
    if num.shape[1] == 0:
        raise ValueError("At least one numeric predictor is required by the anomaly ensemble")

    votes = {}
    votes["IQR"] = union_univariate_flags(
        num,
        lambda c: (c < np.quantile(c, 0.25) - 1.5 * (np.quantile(c, 0.75) - np.quantile(c, 0.25)))
        | (c > np.quantile(c, 0.75) + 1.5 * (np.quantile(c, 0.75) - np.quantile(c, 0.25))),
    )
    votes["Percentile"] = union_univariate_flags(
        num, lambda c: (c < np.quantile(c, 0.05)) | (c > np.quantile(c, 0.95))
    )
    votes["ZScore"] = union_univariate_flags(
        num,
        lambda c: np.abs((c - np.mean(c)) / (np.std(c, ddof=1) if np.std(c, ddof=1) else 1.0))
        > cfg.z_threshold,
    )
    votes["Hampel"] = union_univariate_flags(
        num,
        lambda c: np.abs(c - np.median(c))
        > 3.0 * max(1.4826 * np.median(np.abs(c - np.median(c))), 1e-12),
    )
    votes["Grubbs"] = grubbs_flags(num)

    scaled = StandardScaler().fit_transform(num)
    lof = LocalOutlierFactor(n_neighbors=min(20, n - 1), contamination=cfg.lof_fraction)
    votes["LOF"] = (lof.fit_predict(scaled) == -1).astype(int)
    nbr = NearestNeighbors(n_neighbors=min(6, n)).fit(scaled)
    distances, _ = nbr.kneighbors(scaled)
    votes["KNNOutlier"] = top_fraction(distances[:, 1:].mean(axis=1), cfg.knn_fraction)
    iso = IsolationForest(
        n_estimators=200,
        contamination=cfg.iforest_fraction,
        random_state=seed,
        n_jobs=2,
    )
    votes["IsolationForest"] = (iso.fit_predict(encoded_first) == -1).astype(int)

    if cat.shape[1] > 0:
        k = min(30, max(2, n // 10))
        km = KModes(n_clusters=k, init="Huang", n_init=1, max_iter=20, random_state=seed, verbose=0)
        clusters = km.fit_predict(cat)
        counts = pd.Series(clusters).value_counts().sort_values()
        rare = counts.index[: min(5, len(counts))]
        votes["KModes"] = np.isin(clusters, rare).astype(int)
    else:
        votes["KModes"] = np.zeros(n, dtype=int)

    vote_frame = pd.DataFrame(votes, index=x_first.index)
    entropy = entropy_gain_scores(x_first, plan.numeric_, plan.categorical_)
    return vote_frame, entropy


def shortlist_initial(votes: pd.DataFrame, entropy: np.ndarray, architecture: str, cfg: SADConfig):
    base = votes.sum(axis=1).to_numpy()
    entropy_vote = top_fraction(entropy, cfg.entropy_tail_fraction)
    if architecture == "simple_a":
        return base >= cfg.tad, base / votes.shape[1]
    if architecture == "simple_b":
        total = base + entropy_vote
        return total >= cfg.tad, total / (votes.shape[1] + 1)
    if architecture == "multilayer":
        consensus = base >= cfg.tad
        gate = np.zeros(len(base), dtype=bool)
        candidate_idx = np.flatnonzero(consensus)
        if len(candidate_idx):
            local = top_fraction(entropy[candidate_idx], cfg.entropy_tail_fraction).astype(bool)
            gate[candidate_idx[local]] = True
        return consensus & gate, (base / votes.shape[1]) * (0.5 + 0.5 * gate.astype(float))
    raise ValueError(f"Unknown architecture: {architecture}")


def reviewed_indices(shortlist: np.ndarray, audit_fraction: float, rng: np.random.Generator) -> np.ndarray:
    shortlisted = np.flatnonzero(shortlist)
    potential_normal = np.flatnonzero(~shortlist)
    audit_n = min(len(potential_normal), max(1, int(math.ceil(audit_fraction * len(potential_normal)))))
    audit = rng.choice(potential_normal, size=audit_n, replace=False) if audit_n else np.array([], dtype=int)
    return np.unique(np.concatenate([shortlisted, audit])).astype(int)

