#!/usr/bin/env python3
"""Main run: python 01_Code/run_experiments.py --seeds 10

Existing completed trajectories are reused only if code/data hashes match.
Use --output PATH for a new results directory.
"""
from pathlib import Path
import sys, argparse, json, hashlib, platform, time, importlib.metadata
import numpy as np
import pandas as pd
from dataclasses import replace, asdict
from sad.common import SADConfig, DATASETS
from sad.FinalModeling import prepare, trajectory
from sad.Evaluation import aggregate
from reconcile_exported_scores import reconcile

ROOT = Path(__file__).resolve().parents[1]
def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def main():
    p = argparse.ArgumentParser()
    p.add_argument("--seeds", type=int, default=10)
    p.add_argument("--dataset", choices=list(DATASETS), default=None)
    p.add_argument("--output", type=Path, default=ROOT / "03_Results")
    args = p.parse_args()
    cfg = SADConfig(tad=3, tml=4)
    seeds = list(range(1234, 1234+args.seeds))
    args.output.mkdir(parents=True, exist_ok=True)
    code = {f.name:digest(f) for f in (ROOT/"01_Code/sad").glob("*.py")}
    datahash = {name:digest(ROOT/"02_Data"/spec["filename"]) for name,spec in DATASETS.items()}
    signature = dict(code=code, data=datahash, config=asdict(cfg))
    old = args.output/"signature.json"
    if old.exists() and json.loads(old.read_text()) != json.loads(json.dumps(signature)):
        raise RuntimeError("Code/data changed. Choose a new --output directory.")
    old.write_text(json.dumps(signature, indent=2))
    selected = {args.dataset:DATASETS[args.dataset]} if args.dataset else DATASETS
    all_rows = []
    for dataset, spec in selected.items():
        df = pd.read_csv(ROOT/"02_Data"/spec["filename"])
        for seed in seeds:
            jobs = [("Main",a,cfg) for a in ["simple_a","simple_b","multilayer"]]
            jobs += [("Strict","multilayer",replace(cfg,tad=5,tml=6))]
            destinations = [args.output/dataset/str(seed)/setting/arch for setting,arch,c in jobs]
            prep = None
            if not all((d/"metrics.csv").exists() for d in destinations):
                print(f"{dataset} seed {seed}: preprocess and 9 anomaly detectors", flush=True)
                prep = prepare(df, spec["target"], cfg, seed)
                split = np.concatenate([np.column_stack([idx+1, np.repeat(i+1,len(idx))]) for i,idx in enumerate(prep["parts"])])
                base = args.output/dataset/str(seed); base.mkdir(parents=True, exist_ok=True)
                pd.DataFrame(split,columns=["row_id","partition"]).to_csv(base/"splits.csv",index=False)
                pd.DataFrame(prep["votes"]).assign(row_id=prep["parts"][0]+1,entropy_gain=prep["entropy"]).to_csv(base/"anomaly_votes.csv",index=False)
                meta = dict(numeric=prep["plan"].numeric_,categorical=prep["plan"].categorical_,
                    dropped=prep["plan"].dropped_,encoded_predictors=prep["z"][0].shape[1],
                    anomaly_seconds=prep["ad_seconds"])
                (base/"preprocessing.json").write_text(json.dumps(meta,indent=2))
            for (setting,arch,c),dest in zip(jobs,destinations):
                if not (dest/"metrics.csv").exists():
                    print(f"  {setting} | {arch}",flush=True)
                    rows = trajectory(prep,dataset,seed,arch,c,setting,dest)
                else:
                    rows = pd.read_csv(dest/"metrics.csv").to_dict("records")
                all_rows.extend(rows)
            # Checkpoint visible summaries after every completed seed.
            pd.DataFrame(all_rows).to_csv(args.output/"metrics_by_seed.csv",index=False)
    raw = reconcile(args.output, selected, seeds)
    raw.to_csv(args.output/"metrics_by_seed.csv",index=False)
    summary = raw.groupby(["dataset","setting","architecture","partition"])[
        ["precision","recall","f1","specificity","balanced_accuracy","roc_auc","pr_auc","alert_rate","review_rate","accuracy"]
    ].agg(["mean","std"])
    summary.columns = [f"{a}_{b}" for a,b in summary.columns]
    summary.reset_index().to_csv(args.output/"summary_mean_sd.csv",index=False)
    (args.output/"execution_manifest.json").write_text(json.dumps(dict(
        status="completed",implementation="Python",seeds=seeds,datasets=list(selected),
        protocol=asdict(cfg),primary_setting=[3,4],sensitivity_setting=[5,6],
        python=sys.version,platform=platform.platform(),
        versions={n:importlib.metadata.version(n) for n in ["numpy","pandas","scikit-learn","scipy","imbalanced-learn","xgboost","kmodes"]},
        code_sha256=code,data_sha256=datahash),indent=2))
    print("Experiment complete.",flush=True)

if __name__ == "__main__": main()
