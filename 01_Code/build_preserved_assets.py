"""Restore submitted workflow PNGs verbatim and rebuild the descriptive table."""
from pathlib import Path
import shutil,hashlib,json
import pandas as pd
ROOT=Path(__file__).resolve().parents[1]

def main():
    source=ROOT/'07_Original_Archive/Submitted_V5_Confirmed'
    audit=[]
    for name in ['simple.png','Multilayered.png']:
        src=source/'Pictures'/name
        for folder in ['04_Figures','05_Manuscript/figures']:
            dst=ROOT/folder/name;shutil.copy2(src,dst)
            assert src.read_bytes()==dst.read_bytes()
        audit.append({'source':str(src.relative_to(ROOT)),'figure':name,'byte_identical':True,
                      'sha256':hashlib.sha256(src.read_bytes()).hexdigest()})
    for name in ['Simple.drawio','Multilayered.drawio']:
        shutil.copy2(source/name,ROOT/'04_Figures'/name)
    for folder in [ROOT/'04_Figures',ROOT/'05_Manuscript/figures']:
        for stem in ['01_architecture','10_simple_workflow','11_multilayer_workflow']:
            for p in folder.glob(stem+'.*'):p.unlink()
    (ROOT/'08_Documentation/original_workflows_restored.json').write_text(json.dumps({'status':'PASS','figures':audit},indent=2))
    data=pd.read_csv(ROOT/'02_Data/carclaims.csv')
    rows=[]
    for feature in ['Sex','MaritalStatus','VehicleCategory','DriverRating','FraudFound']:
        for category,n in data[feature].value_counts().sort_index().items():
            rows.append(dict(feature=feature,category=category,frequency=int(n),percentage=100*n/len(data)))
    pd.DataFrame(rows).to_csv(ROOT/'04_Figures/plot_data/original_frequency_table.csv',index=False)
    lines=[r'\begin{table}[tbp]\centering\footnotesize',
           r'\caption{Frequency table for selected categorical variables in Carclaims. Original descriptive variables are retained; percentages are recomputed from the supplied file.}\label{tab:frequency}',
           r'\begin{tabular}{llrr}\toprule Feature & Category & Percent & Frequency\\\midrule']
    previous=None
    for row in rows:
        f=row['feature']
        lines.append(f"{f if f!=previous else ''} & {row['category']} & {row['percentage']:.2f} & {row['frequency']:,}"+r'\\')
        previous=f
    lines.append(r'\bottomrule\end{tabular}\end{table}')
    (ROOT/'05_Manuscript/generated/table_frequency.tex').write_text('\n'.join(lines)+'\n')
    print('Restored the two original workflow PNGs byte-for-byte and regenerated the descriptive table.')

if __name__=='__main__': main()
