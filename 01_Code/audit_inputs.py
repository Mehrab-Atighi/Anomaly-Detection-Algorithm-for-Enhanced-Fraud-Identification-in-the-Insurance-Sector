#!/usr/bin/env python3
"""Document source schemas and the duplicate benchmark; never changes data."""
from pathlib import Path
import json,re
import numpy as np
import pandas as pd
ROOT=Path(__file__).resolve().parents[1]

def main():
    original=ROOT/"07_Original_Archive/Master_Thesis"; docs=ROOT/"08_Documentation"
    a=pd.read_csv(original/"carclaims.csv"); b=pd.read_csv(original/"fraud_oracle.csv")
    normalized=lambda s: re.sub(r"[^a-z0-9]","",s.lower())
    columns={normalized(c):c for c in b.columns}; pairs={}
    assert len(a)==len(b)
    for c in a.columns:
        if c=="FraudFound":continue
        other=columns[normalized(c)]
        aa=a[c].astype(str); bb=b[other].astype(str)
        pairs[c]=dict(other_column=other,exact_rows=int((aa==bb).sum()),
            normalized_rows=int((aa.str.replace(",","",regex=False)==bb.str.replace(",","",regex=False)).sum()))
    matched=int((a.FraudFound.map({"Yes":1,"No":0})==b.FraudFound_P).sum())
    result=dict(rows=len(a),target_identical_rows=matched,
        exact_predictors=sum(v["exact_rows"]==len(a) for v in pairs.values()),
        normalized_predictors=sum(v["normalized_rows"]==len(a) for v in pairs.values()),
        normalization="Match predictor names ignoring punctuation/case; remove thousands commas from values.",predictor_matches=pairs)
    assert result["target_identical_rows"]==15420 and result["normalized_predictors"]==32
    (docs/"duplicate_dataset_audit.json").write_text(json.dumps(result,indent=2))
    for ds,filename,target in [("Carclaims","carclaims.csv","FraudFound"),("InsuranceClaims","insurance_claims.csv","fraud_reported")]:
        d=pd.read_csv(ROOT/"02_Data"/filename); rows=[]
        for c in d:
            s=d[c].replace("?",np.nan)
            rows.append(dict(column=c,role="target" if c==target else "raw_predictor",dtype=str(d[c].dtype),
                unique_values=int(s.nunique()),missing_after_question_mark=int(s.isna().sum())))
        pd.DataFrame(rows).to_csv(docs/f"{ds}_data_dictionary.csv",index=False)
    print("Duplicate check:",matched,"identical labels;",result["exact_predictors"],"exact predictors;",result["normalized_predictors"],"after formatting normalization.")

if __name__=="__main__":main()
