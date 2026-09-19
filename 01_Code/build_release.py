"""Package the complete final research archive, after validation.

Standard library only. Historical source files are never cleaned or edited.
"""
from pathlib import Path
import argparse,hashlib,json,zipfile,datetime,shutil
ROOT=Path(__file__).resolve().parents[1]

def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--output',type=Path,default=ROOT.parent/'SAD_Final_Revision_Original_Structure.zip')
    a=p.parse_args()
    for report in ['verification_report.json','submission_structure_report.json','isolated_submission_compile.json','reuse_integrity_report.json','ggplot2_verification.json','original_workflows_restored.json']:
        status=json.loads((ROOT/'08_Documentation'/report).read_text())
        assert status['status']=='PASS',report
    docs=json.loads((ROOT/'08_Documentation/document_build_report.json').read_text())
    assert len(docs)==5 and all(d['status']=='compiled' for d in docs)
    for folder in [ROOT/'05_Manuscript',ROOT/'06_Reviewer_and_Professor']:
        for path in folder.glob('*'):
            if path.is_file() and path.suffix in {'.aux','.out','.log','.blg','.toc','.lof','.lot','.fls','.fdb_latexmk'}:path.unlink()
    for path in (ROOT/'05_Manuscript/figures').glob('*.pdf'):path.unlink()
    for folder in (ROOT/'01_Code').rglob('__pycache__'):shutil.rmtree(folder)
    for name in ['04_Figures','05_Manuscript','06_Reviewer_and_Professor']:
        for path in (ROOT/name).rglob('*'):
            if path.is_file() and '.pending' in path.name:path.unlink()
    files=sorted(p for p in ROOT.rglob('*') if p.is_file() and p.name!='package_manifest.json')
    entries=[{'path':str(p.relative_to(ROOT)),'bytes':p.stat().st_size,'sha256':hashlib.sha256(p.read_bytes()).hexdigest()} for p in files]
    manifest={'release':'SAD final revision within original manuscript structure',
              'assembled_date':datetime.date.today().isoformat(),
              'implementation':'Python model execution; executed R/ggplot2 final figures; original R sources preserved unchanged',
              'verification_report':'08_Documentation/verification_report.json',
              'manifest_excludes_itself':True,'files':entries}
    mp=ROOT/'package_manifest.json';mp.write_text(json.dumps(manifest,indent=2))
    a.output.parent.mkdir(parents=True,exist_ok=True)
    prefix='SAD_Final_Revision_Original_Structure'
    with zipfile.ZipFile(a.output,'w',compression=zipfile.ZIP_DEFLATED,compresslevel=9) as z:
        for f in files+[mp]:z.write(f,prefix+'/'+str(f.relative_to(ROOT)))
    with zipfile.ZipFile(a.output) as z:assert z.testzip() is None
    print(json.dumps({'file':str(a.output.resolve()),'bytes':a.output.stat().st_size,
                      'files':len(files)+1,'sha256':hashlib.sha256(a.output.read_bytes()).hexdigest()},indent=2))

if __name__=='__main__':main()
