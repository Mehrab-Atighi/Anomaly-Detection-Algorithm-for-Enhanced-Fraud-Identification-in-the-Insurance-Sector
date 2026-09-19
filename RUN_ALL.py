#!/usr/bin/env python3
"""Portable entry point: inspect/rebuild saved results, or repeat experiments."""
from pathlib import Path
import argparse, os, subprocess, sys, shutil

ROOT=Path(__file__).resolve().parent
def main():
    p=argparse.ArgumentParser(description=__doc__)
    g=p.add_mutually_exclusive_group()
    g.add_argument("--rebuild-only",action="store_true",help="Use the included completed results (default).")
    g.add_argument("--rerun",action="store_true",help="Execute ten seeds on both datasets; default destination is 03_Results_reproduced.")
    p.add_argument("--results",type=Path,help="Results location; completed matching trajectories are resumable.")
    p.add_argument("--pdf",action="store_true",help="Also compile all five LaTeX documents; needs TeX Live/MiKTeX.")
    a=p.parse_args()
    result=a.results or ROOT/("03_Results_reproduced" if a.rerun else "03_Results")
    result=result.resolve()
    env=os.environ.copy(); env["OPENBLAS_NUM_THREADS"]="1"; env["OMP_NUM_THREADS"]="1"
    def run(script,*extra):
        subprocess.run([sys.executable,str(ROOT/"01_Code"/script),*map(str,extra)],cwd=ROOT,env=env,check=True)
    if a.rerun: run("run_experiments.py","--seeds",10,"--output",result)
    run("test_invariants.py")
    run("verify_results.py","--results",result)
    run("build_assets.py","--results",result)
    run("build_preserved_assets.py")
    rscript=shutil.which("Rscript")
    if not rscript:
        raise RuntimeError("Install R and ggplot2, jsonlite, svglite; final article figures are rendered with Rscript.")
    subprocess.run([rscript,str(ROOT/"01_Code/Plot_Article_ggplot2.R"),str(result)],cwd=ROOT,env=env,check=True)
    run("verify_ggplot_outputs.py","--results",result)
    run("build_bibliography.py")
    if a.pdf:
        run("build_documents.py")
        run("package_submission.py")
    print("Completed. Figures: 04_Figures; manuscript: 05_Manuscript; audit: 08_Documentation.")

if __name__=="__main__": main()
