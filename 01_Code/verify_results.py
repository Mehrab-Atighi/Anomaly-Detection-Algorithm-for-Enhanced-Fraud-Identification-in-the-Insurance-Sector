#!/usr/bin/env python3
"""Independent numerical audit of persisted outputs and their provenance."""
from pathlib import Path
import hashlib, json, math, argparse
import numpy as np
import pandas as pd
from scipy.stats import rankdata

ROOT=Path(__file__).resolve().parents[1]
KEYS=["dataset","seed","setting","architecture","partition"]

def digest(p): return hashlib.sha256(p.read_bytes()).hexdigest()

def independent_metrics(q):
    y=q.y.to_numpy().astype(bool); p=q.pred.to_numpy().astype(bool)
    tp=int((y&p).sum()); tn=int((~y&~p).sum()); fp=int((~y&p).sum()); fn=int((y&~p).sum())
    divide=lambda a,b: a/b if b else 0.
    recall=divide(tp,tp+fn); specific=divide(tn,tn+fp)
    scores=q.score.to_numpy(); npos=y.sum(); nneg=len(y)-npos
    auc=(rankdata(scores)[y].sum()-npos*(npos+1)/2)/(npos*nneg)
    # AP with score ties grouped as a single threshold.
    order=np.argsort(-scores,kind="stable"); sy=y[order]; ss=scores[order]
    ends=np.r_[np.flatnonzero(np.diff(ss)!=0),len(ss)-1]
    cumulative=np.cumsum(sy)[ends]; recalls=cumulative/npos
    ap=np.sum(np.diff(np.r_[0.,recalls])*cumulative/(ends+1))
    return dict(n=len(y),fraud_n=int(npos),alerted_n=int(p.sum()),TP=tp,TN=tn,FP=fp,FN=fn,
        accuracy=(tp+tn)/len(y),precision=divide(tp,tp+fp),recall=recall,f1=divide(2*tp,2*tp+fp+fn),
        specificity=specific,balanced_accuracy=(recall+specific)/2,roc_auc=auc,pr_auc=ap,
        alert_rate=p.mean(),reviewed_n=int(q.reviewed.sum()),review_rate=q.reviewed.mean())

def main():
    parser=argparse.ArgumentParser(); parser.add_argument("--results",type=Path,default=ROOT/"03_Results")
    args=parser.parse_args(); out=args.results
    manifest=json.loads((out/"execution_manifest.json").read_text())
    assert manifest["status"]=="completed"
    assert manifest["seeds"]==list(range(1234,1244))
    assert set(manifest["datasets"])=={"Carclaims","InsuranceClaims"}
    for filename,sha in manifest["code_sha256"].items(): assert digest(ROOT/"01_Code/sad"/filename)==sha,filename
    specs={"Carclaims":("carclaims.csv","FraudFound"),"InsuranceClaims":("insurance_claims.csv","fraud_reported")}
    truth={}
    for ds,(filename,target) in specs.items():
        assert digest(ROOT/"02_Data"/filename)==manifest["data_sha256"][ds]
        d=pd.read_csv(ROOT/"02_Data"/filename)
        y=d[target].astype(str).str.lower().map({"yes":1,"no":0,"y":1,"n":0,"1":1,"0":0})
        assert not y.isna().any(); truth[ds]=y.to_numpy()
    total_rows=0; metric_checks=0; files=[]; source_metrics=[]; max_error=0.
    for path in sorted(out.glob("*/*/*/*/predictions.csv.gz")):
        ds,seed,setting,folder=path.relative_to(out).parts[:4]; seed=int(seed)
        q=pd.read_csv(path); assert set(q.dataset)=={ds}; assert set(q.seed)=={seed}
        assert set(q.setting)=={setting}; assert np.isfinite(q.score).all()
        assert q.score.between(0,1).all(); assert not q.duplicated(["model","row_id"]).any()
        assert np.array_equal(q.y.to_numpy(),truth[ds][q.row_id.to_numpy()-1])
        assert (q.reviewed>=q.pred).all(); assert (q.audited==(q.reviewed-q.pred)).all()
        splits=pd.read_csv(out/ds/str(seed)/"splits.csv").set_index("row_id")
        assert len(splits)==len(truth[ds]) and splits.index.is_unique
        assert set(splits.index)==set(range(1,len(truth[ds])+1))
        expected=q.row_id.map(splits.partition).map(lambda a:f"D{a}")
        assert np.array_equal(expected.to_numpy(),q.partition.to_numpy())
        for (model,partition),group in q.groupby(["model","partition"]):
            assert len(group)==int((splits.partition==int(partition[1:])).sum())
            assert group.audited.sum()==math.ceil(.1*(len(group)-group.pred.sum()))
        m=pd.read_csv(path.parent/"metrics.csv"); source_metrics.append(m)
        for _,row in m.iterrows():
            v=q[q.model==row.architecture]
            v=v[v.partition!="D1"] if row.partition=="D2-D4" else v[v.partition==row.partition]
            checked=independent_metrics(v)
            for key,value in checked.items():
                error=abs(value-row[key]); max_error=max(max_error,float(error))
                assert np.isclose(value,row[key],atol=1e-10,rtol=1e-10), (path,row.architecture,row.partition,key,value,row[key])
            metric_checks+=1
        arch={"simple_a":"Simple-A","simple_b":"Simple-B","multilayer":"Multilayer"}[folder]
        own=q[q.model==arch]
        times=json.loads((path.parent/"timing.json").read_text())
        for t in times:
            stage=int(t["stage"][1:]); included=stage if t["operation"].startswith("fit_") or t["operation"].startswith("tune_") else stage-1
            mask=own.partition.isin([f"D{x}" for x in range(1,included+1)])&own.reviewed.eq(1)
            assert int(mask.sum())==t["verified_n"]
            assert int(own.loc[mask,"y"].sum())==t["verified_fraud_n"]
        if folder=="multilayer" and setting=="Main":
            a=q[(q.model=="Multilayer")&(q.partition=="D2")].sort_values("row_id")
            b=q[(q.model=="Multilayer-static")&(q.partition=="D2")].sort_values("row_id")
            assert np.array_equal(a.pred.to_numpy(),b.pred.to_numpy())
            # Parallel tree probability reduction can differ by machine epsilon.
            assert np.allclose(a.score.to_numpy(),b.score.to_numpy(),atol=1e-14,rtol=0)
        total_rows+=len(q); files.append(str(path.relative_to(ROOT)))
    assert len(files)==80, len(files)
    whole=pd.read_csv(out/"metrics_by_seed.csv")
    joined=pd.concat(source_metrics,ignore_index=True)
    assert not whole.duplicated(KEYS).any()
    pd.testing.assert_frame_equal(whole.sort_values(KEYS).reset_index(drop=True),joined.sort_values(KEYS).reset_index(drop=True),check_exact=False,atol=1e-12,rtol=1e-12)
    cols=["precision","recall","f1","specificity","balanced_accuracy","roc_auc","pr_auc","alert_rate","review_rate","accuracy"]
    grouped=whole.groupby(["dataset","setting","architecture","partition"])[cols].agg(["mean","std"])
    grouped.columns=[f"{a}_{b}" for a,b in grouped.columns]
    saved=pd.read_csv(out/"summary_mean_sd.csv")
    pd.testing.assert_frame_equal(grouped.reset_index(),saved,check_exact=False,atol=1e-12,rtol=1e-12)
    report=dict(status="PASS",datasets=manifest["datasets"],seeds_per_dataset=10,
        trajectory_files=len(files),prediction_rows_checked=total_rows,metric_rows_recomputed=metric_checks,
        largest_numeric_difference=max_error,checks=["code/data SHA256","outcomes matched to source row IDs",
        "complete disjoint splits","review masks include every alert plus exact audit count",
        "training counts from earlier verified partitions","static and updating D2 predictions identical",
        "manual confusion metrics","rank-based ROC AUC","threshold-grouped average precision",
        "combined metrics and sample-SD summary reconciliation"],
        scope="Numerical and structural checks; does not certify dataset provenance or scientific novelty.")
    target=ROOT/"08_Documentation/verification_report.json"; target.parent.mkdir(exist_ok=True)
    target.write_text(json.dumps(report,indent=2))
    print(json.dumps(report,indent=2))

if __name__=="__main__": main()
