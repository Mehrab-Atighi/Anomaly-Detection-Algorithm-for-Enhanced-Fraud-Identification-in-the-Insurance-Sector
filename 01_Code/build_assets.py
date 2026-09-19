#!/usr/bin/env python3
"""Prepare plot-data CSVs, tables and numerical paragraphs from audited outputs.

Final article graphics are rendered by Plot_Article_ggplot2.R. Optional
--python-preview produces intermediate diagnostics in the Python style.
"""
from pathlib import Path
import argparse, json, shutil, io
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch
from sklearn.metrics import roc_curve, precision_recall_curve

ROOT = Path(__file__).resolve().parents[1]
ARCH = ["Simple-A", "Simple-B", "Multilayer", "Multilayer-static"]
COLORS = ["#247A9C", "#DA8A32", "#785AA6", "#64866D"]
DS = ["Carclaims", "InsuranceClaims"]
DISPLAY = {"Carclaims":"Carclaims", "InsuranceClaims":"Insurance Claims"}
BASELINES = ["LR","SVM","XGB","NB","RF","KNN","AdaBoost","ANN","BalancedRF","PSO-XGB-adapted","Simple-B"]

def tex(text):
    return str(text).replace("_", r"\_").replace("%",r"\%")

def main():
    parser=argparse.ArgumentParser()
    parser.add_argument("--results",type=Path,default=ROOT/"03_Results")
    parser.add_argument("--python-preview",action="store_true")
    args=parser.parse_args()
    manifest=json.loads((args.results/"execution_manifest.json").read_text())
    if manifest["status"] != "completed" or len(manifest["seeds"]) != 10 or set(manifest["datasets"]) != set(DS):
        raise RuntimeError("The article requires completed ten-seed runs on BOTH datasets.")
    raw=pd.read_csv(args.results/"metrics_by_seed.csv")
    figdir=ROOT/"04_Figures"; data=figdir/"plot_data"
    gen=ROOT/"05_Manuscript/generated"; paperfig=ROOT/"05_Manuscript/figures"
    for d in [figdir,data,gen,paperfig]: d.mkdir(parents=True,exist_ok=True)
    pooled=raw[(raw.partition=="D2-D4") & (raw.setting=="Main")]
    mainarch=pooled[pooled.architecture.isin(ARCH)]
    mainarch.to_csv(data/"architecture_metrics.csv",index=False)
    plt.rcParams.update({"font.family":"DejaVu Sans","font.size":10,
        "axes.spines.top":False,"axes.spines.right":False,
        "axes.labelsize":10,"axes.titlesize":11,"legend.fontsize":9,
        "pdf.fonttype":42,"ps.fonttype":42,"savefig.facecolor":"white"})

    def save(fig,name):
        if not args.python_preview:
            plt.close(fig)
            return
        for ext in ["pdf","svg","png"]:
            buffer=io.BytesIO()
            fig.savefig(buffer,format=ext,dpi=300,bbox_inches="tight",pad_inches=.12)
            payload=buffer.getvalue()
            if len(payload)<100: raise RuntimeError(f"Incomplete {name}.{ext}")
            target=figdir/f"{name}.{ext}"
            temporary=target.with_suffix(target.suffix+'.pending')
            temporary.write_bytes(payload);temporary.replace(target)
        shutil.copy2(figdir/f"{name}.png",paperfig/f"{name}.png")
        plt.close(fig)

    def ms(frame,metric):
        return frame[metric].mean(),frame[metric].std(ddof=1)

    def cell(frame,metric,dec=3):
        a,b=ms(frame,metric)
        return f"${a:.{dec}f} \\pm {b:.{dec}f}$"

    def select(ds,arch,setting="Main",partition="D2-D4"):
        return raw[(raw.dataset==ds)&(raw.architecture==arch)&(raw.setting==setting)&(raw.partition==partition)]

    def table(filename,caption,label,headers,rows,spec=None):
        spec=spec or ("l"+"r"*(len(headers)-1))
        s="\\begin{table}[!htbp]\\centering\\footnotesize\n"
        s+=f"\\caption{{{caption}}}\\label{{{label}}}\n"
        s+="\\setlength{\\tabcolsep}{2pt}\n\\begin{tabular}{"+spec+"}\\toprule\n"
        s+=" & ".join(headers)+r"\\\midrule"+"\n"
        for row in rows:
            if isinstance(row,str): s+="\\multicolumn{"+str(len(headers))+"}{l}{\\textit{"+row+"}}\\\\\n"
            else: s+=" & ".join(row)+r"\\"+"\n"
        s+="\\bottomrule\\end{tabular}\n\\end{table}\n"
        (gen/filename).write_text(s)
        return s

    # 1. Code-generated architecture, not a rasterized screenshot.
    fig,ax=plt.subplots(figsize=(9,6.2)); ax.set(xlim=(0,10),ylim=(0,10)); ax.axis("off")
    def box(x,y,w,h,text,color="#EAF2F6"):
        ax.add_patch(FancyBboxPatch((x,y),w,h,boxstyle="round,pad=.10",linewidth=1,
                                   edgecolor="#446070",facecolor=color))
        ax.text(x+w/2,y+h/2,text,ha="center",va="center",fontsize=10)
    def arrow(a,b): ax.annotate("",xy=b,xytext=a,arrowprops={"arrowstyle":"->","color":"#446070","lw":1.5})
    box(1.8,8.5,6.4,.9,"D1: 50%  |  fit preprocessing without target")
    box(.1,6.7,2.9,1.1,"Simple-A\n9 anomaly detectors\n≥ TAD votes")
    box(3.55,6.7,2.9,1.1,"Simple-B\n9 detectors + entropy\n≥ TAD votes")
    box(7,6.7,2.9,1.1,"Multilayer\n9-detector consensus\n→ candidate-local entropy")
    for x in [1.55,5,8.45]: arrow((5,8.5),(x,7.9))
    box(1.8,4.9,6.4,1.0,"Review every alert + random 10% of non-alerts\nReveal stored labels only after selection", "#FBF0DD")
    for x in [1.55,5,8.45]: arrow((x,6.6),(5,6.0))
    box(1.8,3.3,6.4,1.0,"Verified records → training-only SMOTE\nFit 8 classifiers; ≥ TML hard votes makes an alert")
    arrow((5,4.8),(5,4.4))
    box(.35,1.35,2.75,1.1,"Predict D2: 30%\nReview → update")
    box(3.6,1.35,2.75,1.1,"Predict D3: 10%\nReview → update")
    box(6.85,1.35,2.75,1.1,"Predict D4: 10%\nFinal evaluation")
    arrow((5,3.2),(1.73,2.55)); arrow((3.2,1.9),(3.5,1.9)); arrow((6.45,1.9),(6.75,1.9))
    ax.text(5,.35,"Static ablation: freeze D1 models and predict the identical D2–D4 records.",ha="center",fontsize=10)
    save(fig,"01_architecture")

    # 2. Architecture performance; all intervals are sample SD, not CI.
    fig,axs=plt.subplots(2,3,figsize=(10,6.3),sharey=True,layout="constrained")
    for i,ds in enumerate(DS):
        for j,(metric,title) in enumerate([("recall","Fraud recall"),("f1","Fraud F1"),("pr_auc","Average precision")]):
            ax=axs[i,j]
            for k,arch in enumerate(ARCH):
                a,b=ms(select(ds,arch),metric)
                ax.errorbar(k,a,yerr=b,fmt="o",capsize=4,color=COLORS[k],ms=6)
            ax.set(xticks=range(4),xticklabels=["A","B","Multi","Static"],ylim=(0,1),title=f"{DISPLAY[ds]} · {title}")
            ax.grid(axis="y",alpha=.2)
    fig.supxlabel("A = Simple-A; B = Simple-B; Multi = updating Multilayer; Static = frozen Multilayer",fontsize=9)
    save(fig,"02_architecture_performance")

    # 3. Sequential batches are different samples, not a fixed test learning curve.
    partition=raw[(raw.setting=="Main")&raw.architecture.isin(ARCH)&raw.partition.isin(["D2","D3","D4"])]
    partition.to_csv(data/"partition_metrics.csv",index=False)
    fig,axs=plt.subplots(2,2,figsize=(9,6.5),sharex=True,sharey=True,layout="constrained")
    for i,ds in enumerate(DS):
        for j,metric in enumerate(["f1","recall"]):
            ax=axs[i,j]
            for k,arch in enumerate(ARCH):
                vals=[ms(select(ds,arch,partition=f"D{p}"),metric) for p in [2,3,4]]
                ax.errorbar(np.arange(3)+(k-1.5)*.035,[v[0] for v in vals],yerr=[v[1] for v in vals],
                    label=arch,color=COLORS[k],marker="o",capsize=2,lw=1.3)
            ax.set(xticks=range(3),xticklabels=["D2","D3","D4"],ylim=(0,1),title=f"{DISPLAY[ds]} · {'F1' if metric=='f1' else 'Recall'}")
            ax.grid(axis="y",alpha=.2)
    fig.legend(*axs[0,0].get_legend_handles_labels(),loc="outside lower center",ncol=4,frameon=False)
    save(fig,"03_partition_progress")

    # 4. Separate alert workload from actual oracle queries (audit included).
    fig,axs=plt.subplots(1,2,figsize=(9,4.1),sharey=True,layout="constrained")
    for ax,ds in zip(axs,DS):
        alerts=[select(ds,a).alert_rate.mean() for a in ARCH]
        review=[select(ds,a).review_rate.mean() for a in ARCH]
        ax.bar(range(4),alerts,color=COLORS,label="Alerts")
        ax.bar(range(4),np.array(review)-alerts,bottom=alerts,color="#D4DCE1",label="Non-alert audit")
        for k,v in enumerate(review): ax.text(k,v+.015,f"{v:.1%}",ha="center",fontsize=9)
        ax.set(xticks=range(4),xticklabels=["Simple-A","Simple-B","Multi","Static"],ylim=(0,min(1,max(review)+.15)),title=DISPLAY[ds],ylabel="Fraction of D2–D4 claims")
        ax.yaxis.set_major_formatter(matplotlib.ticker.PercentFormatter(1)); ax.grid(axis="y",alpha=.2)
    axs[1].legend(loc="upper right",frameon=False)
    save(fig,"04_workload")

    # 5. Conditional comparisons on Simple-B's acquired labels.
    missing=set(BASELINES)-set(pooled.architecture)
    if missing: raise RuntimeError(f"Comparator name mismatch: {missing}")
    pooled[pooled.architecture.isin(BASELINES)].to_csv(data/"baseline_metrics.csv",index=False)
    fig,axs=plt.subplots(1,2,figsize=(10,6),sharey=True,layout="constrained")
    y=np.arange(len(BASELINES))
    for ax,ds in zip(axs,DS):
        for off,metric,color,label in [(-.12,"f1",COLORS[0],"F1"),(.12,"pr_auc",COLORS[1],"AP")]:
            vals=[ms(select(ds,a),metric) for a in BASELINES]
            ax.errorbar([v[0] for v in vals],y+off,xerr=[v[1] for v in vals],fmt="o",ms=4,capsize=2,label=label,color=color)
        ax.set(yticks=y,yticklabels=BASELINES,xlim=(0,1),title=DISPLAY[ds],xlabel="Mean ± sample SD")
        ax.grid(axis="x",alpha=.2); ax.legend(frameon=False)
    axs[0].invert_yaxis(); save(fig,"05_baseline_comparison")

    # 6. Predefined representative split, no cherry-picked best seed.
    fig,axs=plt.subplots(2,2,figsize=(9,7),layout="constrained")
    curve_rows=[]
    for i,ds in enumerate(DS):
        for k,arch in enumerate(ARCH):
            folder={"Simple-A":"simple_a","Simple-B":"simple_b","Multilayer":"multilayer","Multilayer-static":"multilayer"}[arch]
            p=pd.read_csv(args.results/ds/"1234"/"Main"/folder/"predictions.csv.gz")
            p=p[(p.model==arch)&(p.partition!="D1")]
            fpr,tpr,_=roc_curve(p.y,p.score); prec,rec,_=precision_recall_curve(p.y,p.score)
            axs[i,0].plot(fpr,tpr,color=COLORS[k],label=arch,lw=1.4)
            axs[i,1].plot(rec,prec,color=COLORS[k],label=arch,lw=1.4)
            for typ,x,yy in [("ROC",fpr,tpr),("PR",rec,prec)]:
                curve_rows.extend(dict(dataset=ds,architecture=arch,curve=typ,x=a,y=b) for a,b in zip(x,yy))
        axs[i,0].plot([0,1],[0,1],"--",color="gray",lw=1)
        axs[i,1].axhline(p.y.mean(),ls="--",color="gray",lw=1)
        axs[i,0].set(title=DISPLAY[ds]+" · ROC",xlabel="False-positive rate",ylabel="True-positive rate",xlim=(0,1),ylim=(0,1.02))
        axs[i,1].set(title=DISPLAY[ds]+" · Precision–recall",xlabel="Recall",ylabel="Precision",xlim=(0,1),ylim=(0,1.02))
    fig.legend(*axs[0,0].get_legend_handles_labels(),loc="outside lower center",ncol=4,frameon=False)
    pd.DataFrame(curve_rows).to_csv(data/"representative_curves_seed1234.csv",index=False)
    save(fig,"06_roc_pr")

    # 7. True sequential ablation: same initial fit, no later retraining.
    paired=[]
    for ds in DS:
        a=select(ds,"Multilayer").set_index("seed")
        b=select(ds,"Multilayer-static").set_index("seed")
        for seed in a.index:
            paired.append(dict(dataset=ds,seed=seed,**{m:a.loc[seed,m]-b.loc[seed,m] for m in ["f1","pr_auc","review_rate"]}))
    delta=pd.DataFrame(paired); delta.to_csv(data/"paired_ablation_deltas.csv",index=False)
    fig,axs=plt.subplots(1,3,figsize=(10,3.8),layout="constrained")
    for ax,metric,title in zip(axs,["f1","pr_auc","review_rate"],["Δ F1","Δ AP","Δ review rate"]):
        for i,ds in enumerate(DS):
            vals=delta.loc[delta.dataset==ds,metric].to_numpy()
            ax.scatter(i+np.linspace(-.12,.12,len(vals)),vals,s=18,alpha=.6,color=COLORS[i])
            ax.errorbar(i,vals.mean(),yerr=vals.std(ddof=1),fmt="D",color="#263745",capsize=5,ms=5)
        ax.axhline(0,color="gray",ls="--",lw=1)
        ax.set(title=title,xticks=[0,1],xticklabels=["Carclaims","Insurance\nClaims"],ylabel="Updating minus frozen")
    save(fig,"07_ablation_deltas")

    # 8. Sensitivity is full trajectories at TWO pairs, never a fabricated grid.
    sens=raw[(raw.architecture=="Multilayer")&(raw.partition=="D2-D4")]
    sens.to_csv(data/"threshold_metrics.csv",index=False)
    fig,axs=plt.subplots(1,2,figsize=(9,4.3),sharey=True,layout="constrained")
    for ax,ds in zip(axs,DS):
        for k,(setting,color) in enumerate([("Main",COLORS[2]),("Strict",COLORS[0])]):
            vals=[ms(select(ds,"Multilayer",setting),m) for m in ["recall","f1","pr_auc","review_rate"]]
            ax.bar(np.arange(4)+(k-.5)*.32,[v[0] for v in vals],width=.3,yerr=[v[1] for v in vals],capsize=3,color=color,
                   label="(TAD, TML) = "+("(3, 4)" if setting=="Main" else "(5, 6)"))
        ax.set(xticks=range(4),xticklabels=["Recall","F1","AP","Review"],ylim=(0,1),title=DISPLAY[ds]); ax.grid(axis="y",alpha=.2)
    fig.legend(*axs[0].get_legend_handles_labels(),loc="outside lower center",ncol=2,frameon=False)
    save(fig,"08_threshold_sensitivity")

    # 9. Wall time definitions preserve the boundary of timed operations.
    times=[]; ad=[]
    for ds in DS:
        for seed in manifest["seeds"]:
            pre=json.loads((args.results/ds/str(seed)/"preprocessing.json").read_text())
            ad.append(dict(dataset=ds,seed=seed,seconds=pre["anomaly_seconds"]))
            for arch,folder in [("Simple-A","simple_a"),("Simple-B","simple_b"),("Multilayer","multilayer")]:
                tt=json.loads((args.results/ds/str(seed)/"Main"/folder/"timing.json").read_text())
                times.append(dict(dataset=ds,seed=seed,architecture=arch,
                    seconds=sum(x["seconds"] for x in tt if x["operation"]=="fit_8_models")))
    times=pd.DataFrame(times); ad=pd.DataFrame(ad)
    times.to_csv(data/"supervised_fit_seconds.csv",index=False); ad.to_csv(data/"shared_anomaly_seconds.csv",index=False)
    fig,axs=plt.subplots(1,2,figsize=(9,3.5),layout="constrained")
    for ax,ds in zip(axs,DS):
        for k,arch in enumerate(ARCH[:3]):
            vals=times.loc[(times.dataset==ds)&(times.architecture==arch),"seconds"]
            ax.barh(k,vals.mean(),xerr=vals.std(),color=COLORS[k],capsize=4)
        ax.set(yticks=range(3),yticklabels=ARCH[:3],xlabel="Three fits of 8 classifiers (seconds)",title=DISPLAY[ds]); ax.invert_yaxis()
        ax.grid(axis="x",alpha=.2)
    save(fig,"09_runtime")

    rows=[]; second=[]
    for ds in DS:
        rows.append(DISPLAY[ds]); second.append(DISPLAY[ds])
        for arch in ARCH:
            q=select(ds,arch)
            rows.append([tex(arch)]+[cell(q,m) for m in ["precision","recall","f1","pr_auc","review_rate"]])
            second.append([tex(arch)]+[cell(q,m) for m in ["specificity","balanced_accuracy","roc_auc","accuracy"]])
    first=table("table_architectures.tex","Fraud-class performance and total review fraction; mean $\\pm$ sample SD across ten seeds. All metrics pool $D_2$--$D_4$ within a seed. AP denotes average precision.","tab:architectures",["Architecture","Precision","Recall","F1","AP","Review"],rows)
    ss=table("table_architectures_aux.tex","Complementary metrics under the same main protocol. High accuracy alone is insufficient for these imbalanced datasets.","tab:architectures_aux",["Architecture","Specificity","Balanced acc.","ROC-AUC","Accuracy"],second)
    (gen/"table_architectures.tex").write_text(first+ss)

    rows=[]
    for arch in BASELINES:
        rows.append([tex(arch)]+[cell(select(ds,arch),m) for ds in DS for m in ["f1","pr_auc"]])
    table("table_baselines.tex","Conditional classifier comparison using Simple-B's verified records. The two columns per benchmark report fraud F1 and AP. PSO-XGB is the explicitly adapted comparator.","tab:baselines",["Method","Carclaims F1","Carclaims AP","Insurance F1","Insurance AP"],rows)
    table("table_ablation.tex","Paired seed-wise change from static to updating Multilayer: mean $\\pm$ sample SD. These are descriptive paired differences, not confidence intervals.","tab:ablation",["Dataset",r"$\Delta$ F1",r"$\Delta$ AP",r"$\Delta$ review"],
          [[DISPLAY[ds]]+[cell(delta[delta.dataset==ds],m,4) for m in ["f1","pr_auc","review_rate"]] for ds in DS])
    table("table_thresholds.tex","Multilayer sensitivity at two joint threshold settings; mean $\\pm$ sample SD.","tab:thresholds",["Dataset / thresholds","Recall","F1","AP","Review"],
          [[DISPLAY[ds]+f" / {pair}"]+[cell(select(ds,"Multilayer",setting),m) for m in ["recall","f1","pr_auc","review_rate"]]
           for ds in DS for setting,pair in [("Main","(3,4)"),("Strict","(5,6)")]])
    rows=[[a]+[cell(times[(times.dataset==ds)&(times.architecture==a)],"seconds",2) for ds in DS] for a in ARCH[:3]]
    rows.append(["Shared anomaly stage"]+[cell(ad[ad.dataset==ds],"seconds",2) for ds in DS])
    table("table_runtime.tex","Wall time in seconds, mean $\\pm$ sample SD. Architecture rows sum supervised fitting at $D_1,D_2,D_3$; the shared anomaly stage is reported separately and excludes preprocessing.","tab:runtime",["Operation","Carclaims","Insurance Claims"],rows)

    b=select("InsuranceClaims","Simple-B"); m=select("InsuranceClaims","Multilayer")
    nums={"ExtSimpleBFone":b.f1.mean(),"ExtSimpleBAP":b.pr_auc.mean(),"ExtMultiFone":m.f1.mean(),"ExtMultiReview":m.review_rate.mean()}
    (gen/"numbers.tex").write_text("% AUTO-GENERATED from actual ten-seed results.\n"+"\n".join(f"\\newcommand{{\\{k}}}{{{v:.3f}}}" for k,v in nums.items())+"\n")
    paragraphs=[]
    for ds in DS:
        best=mainarch[mainarch.dataset==ds].groupby("architecture").f1.mean().idxmax()
        q=select(ds,best); mm=select(ds,"Multilayer"); sb=select(ds,"Simple-B")
        paragraphs.append(f"On {DISPLAY[ds]}, {best} has the highest mean F1 among the four architecture variants ({q.f1.mean():.3f}). "
            f"Simple-B and updating Multilayer attain AP {sb.pr_auc.mean():.3f} and {mm.pr_auc.mean():.3f}, respectively; "
            f"Multilayer's mean F1 is {mm.f1.mean():.3f}. "
            f"The corresponding review fractions are {sb.review_rate.mean():.3f} for Simple-B and {mm.review_rate.mean():.3f} for Multilayer.")
    paragraphs.append("These differences concern both predictive performance and the records acquired for training. The architectures are therefore compared as complete review policies, without holding the review budget constant.")
    (gen/"results_architectures.tex").write_text("\n\n".join(paragraphs)+"\n")
    paragraphs=[]
    for ds in DS:
        q=pooled[(pooled.dataset==ds)&pooled.architecture.isin(BASELINES)]
        means=q.groupby("architecture")[["f1","pr_auc"]].mean()
        best_f1=means.f1.idxmax();best_ap=means.pr_auc.idxmax()
        if best_f1==best_ap:
            paragraphs.append(f"For {DISPLAY[ds]}, {best_f1} leads this comparator set in both mean F1 ({means.loc[best_f1,'f1']:.3f}) and AP ({means.loc[best_ap,'pr_auc']:.3f}).")
        else:
            paragraphs.append(f"For {DISPLAY[ds]}, {best_f1} has the highest mean F1 ({means.loc[best_f1,'f1']:.3f}), whereas {best_ap} has the highest AP ({means.loc[best_ap,'pr_auc']:.3f}).")
    paragraphs.append("These rankings are conditional on Simple-B's acquisition policy and the stated training settings. Differences in tuning budgets limit comparisons with fully optimized implementations of the published methods.")
    (gen/"results_baselines.tex").write_text(" ".join(paragraphs)+"\n")
    paragraphs=[]
    for ds in DS:
        q=delta[delta.dataset==ds]
        paragraphs.append(f"For {DISPLAY[ds]}, sequential updating changes mean F1 by {q.f1.mean():+.4f}, AP by {q.pr_auc.mean():+.4f} and review fraction by {q.review_rate.mean():+.4f} relative to the frozen model.")
    paragraphs.append("The Carclaims F1 change is small relative to its split-to-split variation. These descriptive results do not establish statistical superiority. Because updating affects later acquisition as well as prediction, the ablation measures the complete update policy. The two threshold settings are evaluated through separate complete trajectories.")
    (gen/"results_ablation.tex").write_text(" ".join(paragraphs)+"\n")
    (gen/"results_conclusion.tex").write_text(
        f"On the second benchmark, Simple-B attains mean fraud F1 {b.f1.mean():.3f} and AP {b.pr_auc.mean():.3f}; "
        f"updating Multilayer attains F1 {m.f1.mean():.3f} at review fraction {m.review_rate.mean():.3f}. "
        "These values demonstrate an empirical workload--detection trade-off rather than a universal benefit of adding an entropy gate.\n")
    print("Prepared plot-data CSVs, article tables and numerical text; final rendering uses Plot_Article_ggplot2.R.")

if __name__=="__main__": main()
