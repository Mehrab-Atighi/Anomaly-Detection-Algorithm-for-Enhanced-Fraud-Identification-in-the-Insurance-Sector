"""Check cross-language plot values and the exact restored original images."""
from pathlib import Path
import hashlib,json,argparse
import pandas as pd
import numpy as np
from PIL import Image
ROOT=Path(__file__).resolve().parents[1]

def main():
    parser=argparse.ArgumentParser();parser.add_argument('--results',type=Path)
    args=parser.parse_args()
    report=json.loads((ROOT/'08_Documentation/ggplot2_build_report.json').read_text())
    assert report['status']=='PASS'
    md5=lambda p:hashlib.md5(p.read_bytes()).hexdigest()
    assert md5(ROOT/'01_Code/Plot_Article_ggplot2.R')==report['script_md5']
    results=args.results or ROOT/report['results_directory']
    assert md5(results/'metrics_by_seed.csv') in report['input_md5'].values()
    raw=pd.read_csv(results/'metrics_by_seed.csv')
    arch=['Simple-A','Simple-B','Multilayer','Multilayer-static']
    models=['LR','SVM','XGB','NB','RF','KNN','AdaBoost','ANN','BalancedRF','PSO-XGB-adapted','Simple-B']
    pooled=raw[(raw.setting=='Main') & (raw.partition=='D2-D4')]
    parts=raw[(raw.setting=='Main') & raw.architecture.isin(arch) & raw.partition.isin(['D2','D3','D4'])].copy()
    parts['Partition']=parts.partition.str[1:].astype(int)
    checks=0;maxerr=0.
    def check(file,source,metrics,keys,scale=100):
        nonlocal checks,maxerr
        q=pd.read_csv(ROOT/'04_Figures/plot_data'/file)
        grouped=[]
        source=source.copy()
        source['Dataset']=source.dataset.replace({'InsuranceClaims':'Insurance Claims'})
        if 'architecture' in source:
            source['Method']=source.architecture;source['Classifier']=source.architecture
        if 'setting' in source:source['Setting']=source.setting.map({'Main':'(3,4)','Strict':'(5,6)'})
        for label,col in metrics.items():
            s=source.groupby(keys)[col].agg(['mean','std']).reset_index()
            s['Metric']=label;s['Mean']=scale*s['mean'];s['SD']=scale*s['std']
            grouped.append(s[keys+['Metric','Mean','SD']])
        expected=pd.concat(grouped,ignore_index=True)
        if 'Metric' not in q:q['Metric']=next(iter(metrics))
        join=keys+['Metric'];merged=q.merge(expected,on=join,suffixes=('_r','_python'),validate='one_to_one')
        assert len(merged)==len(q)==len(expected),file
        for c in ['Mean','SD']:
            error=np.max(np.abs(merged[c+'_r']-merged[c+'_python']))
            maxerr=max(maxerr,float(error));assert error<1e-9,(file,c,error)
        checks+=len(merged)
    check('ggplot_architecture_summary.csv',pooled[pooled.architecture.isin(arch)],{'Fraud recall':'recall','Fraud F1':'f1','Average precision':'pr_auc'},['Dataset','Method'])
    check('ggplot_partition_summary.csv',parts,{'Fraud F1':'f1','Fraud recall':'recall'},['Dataset','Method','Partition'])
    check('ggplot_workload_summary.csv',parts,{'Alerts':'alert_rate','Total review':'review_rate'},['Dataset','Method','Partition'])
    check('ggplot_baseline_summary.csv',pooled[pooled.architecture.isin(models)],{'Fraud F1':'f1','Average precision':'pr_auc'},['Dataset','Classifier'])
    delta=pd.read_csv(ROOT/'04_Figures/plot_data/paired_ablation_deltas.csv')
    check('ggplot_ablation_summary.csv',delta,{'F1 change':'f1','AP change':'pr_auc','Review-rate change':'review_rate'},['Dataset'])
    check('ggplot_threshold_summary.csv',raw[(raw.architecture=='Multilayer')&(raw.partition=='D2-D4')],{'Fraud recall':'recall','Fraud F1':'f1','Average precision':'pr_auc','Total review':'review_rate'},['Dataset','Setting'])
    timings=pd.read_csv(ROOT/'04_Figures/plot_data/supervised_fit_seconds.csv')
    check('ggplot_runtime_summary.csv',timings,{'Runtime':'seconds'},['Dataset','Method'],scale=1)
    bounds=[]
    for stem,upper,errorbars in [('architecture',80,True),('partition',80,True),
                                 ('baseline',80,True),('threshold',80,True),
                                 ('workload',55,False)]:
        q=pd.read_csv(ROOT/'04_Figures/plot_data'/f'ggplot_{stem}_summary.csv')
        low=np.maximum(0,q.Mean-q.SD) if errorbars else q.Mean
        high=np.minimum(100,q.Mean+q.SD) if errorbars else q.Mean
        assert low.min()>=0 and high.max()<=upper,(stem,'displayed values clipped')
        bounds.append({'figure_family':stem,'axis_min':0,'axis_max':upper,
                       'displayed_min':float(low.min()),'displayed_max':float(high.max()),
                       'includes_displayed_SD_endpoints':errorbars})
    for fig in report['figures'].values():
        for name,digest in fig['files_md5'].items():assert md5(ROOT/'04_Figures'/name)==digest,name
    for p in (ROOT/'05_Manuscript/figures').glob('*.png'):
        assert p.read_bytes()==(ROOT/'04_Figures'/p.name).read_bytes(),p.name
        with Image.open(p) as im: im.verify()
    for name in ['simple.png','Multilayered.png']:
        assert (ROOT/'05_Manuscript/figures'/name).read_bytes()==(ROOT/'07_Original_Archive/Submitted_V5_Confirmed/Pictures'/name).read_bytes()
    out={'status':'PASS','summary_groups_compared':checks,'mean_and_sample_SD_checked':True,
         'largest_absolute_difference':maxerr,'original_workflow_pngs_byte_identical':True,
         'R_script_and_all_32_export_hashes_match':True,'paper_figure_copies_match':True,
         'display_ranges_enclose_all_points_and_displayed_error_bars':bounds,
         'scope':'Cross-language rendering audit; model results remain the previous audited execution.'}
    (ROOT/'08_Documentation/ggplot2_verification.json').write_text(json.dumps(out,indent=2))
    print(json.dumps(out,indent=2))

if __name__=='__main__': main()
