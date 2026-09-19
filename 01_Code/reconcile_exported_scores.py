"""Recompute ranking metrics from the exact scores distributed in CSV.

The model writer uses 16 significant digits. Floating-point ties can change
when CSV scores are read back. Original in-memory metrics are preserved;
the published metrics use the distributed score representation consistently.
Confusion-based metrics are unaffected. This is numerical reconciliation,
not model selection or replacement of predictions.
"""
import json
from pathlib import Path
import numpy as np
import pandas as pd
from sklearn.metrics import roc_auc_score, average_precision_score

def reconcile(results, datasets, seeds):
    tables=[]; changes=[]
    for ds in datasets:
        for seed in seeds:
            for path in sorted((Path(results)/ds/str(seed)).glob("*/*/metrics.csv")):
                original=path.with_name("metrics_before_csv_roundtrip.csv")
                if not original.exists(): original.write_bytes(path.read_bytes())
                m=pd.read_csv(original); q=pd.read_csv(path.with_name("predictions.csv.gz"))
                for i,row in m.iterrows():
                    selected=q[q.model==row.architecture]
                    selected=selected[selected.partition!="D1"] if row.partition=="D2-D4" else selected[selected.partition==row.partition]
                    for key,fn in [("roc_auc",roc_auc_score),("pr_auc",average_precision_score)]:
                        value=fn(selected.y,selected.score)
                        changes.append(dict(file=str(path.relative_to(results)),metric=key,
                            absolute_difference=abs(value-row[key])))
                        m.loc[i,key]=value
                m.to_csv(path,index=False); tables.append(m)
    audit=pd.DataFrame(changes)
    audit.groupby("metric").absolute_difference.agg(["max","mean"]).to_csv(Path(results)/"score_serialization_audit.csv")
    return pd.concat(tables,ignore_index=True)

if __name__=="__main__":
    root=Path(__file__).resolve().parents[1]/"03_Results"
    manifest=json.loads((root/"execution_manifest.json").read_text())
    raw=reconcile(root,manifest["datasets"],manifest["seeds"])
    raw.to_csv(root/"metrics_by_seed.csv",index=False)
    cols=["precision","recall","f1","specificity","balanced_accuracy","roc_auc","pr_auc","alert_rate","review_rate","accuracy"]
    summary=raw.groupby(["dataset","setting","architecture","partition"])[cols].agg(["mean","std"])
    summary.columns=[f"{a}_{b}" for a,b in summary.columns]
    summary.reset_index().to_csv(root/"summary_mean_sd.csv",index=False)
    print((root/"score_serialization_audit.csv").read_text())
