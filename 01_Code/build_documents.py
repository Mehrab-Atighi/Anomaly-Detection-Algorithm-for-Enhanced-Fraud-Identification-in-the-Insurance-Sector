#!/usr/bin/env python3
"""Build clean/marked article, response, change map and Persian report."""
from pathlib import Path
import shutil, subprocess, json, re, tempfile
ROOT=Path(__file__).resolve().parents[1]

def build_in_staging(stage):
    required=["pdflatex","bibtex","xelatex"]
    missing=[a for a in required if not shutil.which(a)]
    if missing: raise RuntimeError("Install a TeX distribution providing: "+", ".join(missing))
    logs=ROOT/"08_Documentation/build_logs"; logs.mkdir(exist_ok=True,parents=True)
    status=[]
    tasks=[(stage/"05_Manuscript",n,"pdflatex",True) for n in ["manuscript_clean","manuscript_marked"]]
    tasks += [(stage/"06_Reviewer_and_Professor",n,"pdflatex",False) for n in ["response_to_reviewer","change_map"]]
    tasks += [(stage/"06_Reviewer_and_Professor","professor_report_fa","xelatex",False)]
    for folder,name,engine,bib in tasks:
        # Fresh auxiliary state also recovers from a previously interrupted TeX run.
        for suffix in [".aux",".out",".toc",".lof",".lot",".bbl",".blg",".log"]:
            (folder/(name+suffix)).unlink(missing_ok=True)
        commands=[[engine,"-interaction=nonstopmode","-halt-on-error",name+".tex"]]
        if bib: commands += [["bibtex",name]]
        commands += [[engine,"-interaction=nonstopmode","-halt-on-error",name+".tex"]]*2
        with (logs/(name+"_build.txt")).open("w") as f:
            for cmd in commands: subprocess.run(cmd,cwd=folder,stdout=f,stderr=subprocess.STDOUT,check=True)
            for repeat in range(3):
                latest=(folder/(name+".log")).read_text(errors="replace")
                if "Rerun to get cross-references right" not in latest: break
                subprocess.run([engine,"-interaction=nonstopmode","-halt-on-error",name+".tex"],cwd=folder,stdout=f,stderr=subprocess.STDOUT,check=True)
        text=(folder/(name+".log")).read_text(errors="replace")
        defects=[line for line in text.splitlines() if any(x in line for x in ["undefined references","undefined citations","Overfull ","Missing character:","Rerun to get cross-references right"])]
        if defects: raise RuntimeError(f"Layout/reference issue in {name}: "+"; ".join(defects))
        pdf=(folder/(name+".pdf")).read_bytes()
        if not pdf.startswith(b"%PDF-") or b"%%EOF" not in pdf[-1024:]:
            raise RuntimeError(f"Incomplete PDF output: {name}")
        pages=re.search(r"Output written on .*?\((\d+) pages?",text,re.S)
        if not pages or int(pages.group(1))<1: raise RuntimeError(f"No compiled pages: {name}")
        # Publish only finished files, so readers never see an in-progress PDF.
        destination=ROOT/folder.relative_to(stage)
        for suffix in [".pdf", ".bbl"]:
            source=folder/(name+suffix)
            if source.exists():
                pending=destination/(name+suffix+".pending")
                pending.write_bytes(source.read_bytes())
                pending.replace(destination/source.name)
        status.append(dict(document=str((destination/(name+".pdf")).relative_to(ROOT)),compiler=engine,status="compiled",pages=int(pages.group(1)),checked="complete PDF; no undefined references/citations, overfull boxes or missing glyphs"))
        print("Compiled",name,flush=True)
    (ROOT/"08_Documentation/document_build_report.json").write_text(json.dumps(status,indent=2))

def main():
    with tempfile.TemporaryDirectory(prefix="sad_document_build_") as directory:
        stage=Path(directory)
        for name in ["05_Manuscript", "06_Reviewer_and_Professor"]:
            shutil.copytree(ROOT/name,stage/name)
        build_in_staging(stage)

if __name__=="__main__": main()
