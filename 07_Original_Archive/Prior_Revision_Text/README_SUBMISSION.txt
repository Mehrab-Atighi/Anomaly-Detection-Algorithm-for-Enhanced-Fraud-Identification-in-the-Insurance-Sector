ANNALS OF DATA SCIENCE — MAJOR REVISION PACKAGE
Manuscript: Anomaly Detection Algorithm for Enhanced Fraud Identification in the Insurance Sector

IMPORTANT SUBMISSION NOTE
The editor explicitly requested editable source files only and stated that PDF is not allowed at this revision stage. Upload the archive named:
JOURNAL_UPLOAD_EDITABLE_SOURCE_ONLY.zip

CONTENTS OF THE SOURCE-ONLY ARCHIVE
- main.tex: default entry point; it opens the clean revised manuscript.
- manuscript_revised_clean.tex: clean revised manuscript.
- manuscript_revised_highlighted.tex: revised manuscript with yellow markers identifying every added or replaced block.
- response_to_reviewer.tex: point-by-point response to Reviewer #1.
- refs.bib: updated bibliography, including recent 2024–2026 insurance-fraud studies.
- latexmkrc: automatic multi-pass LaTeX/BibTeX build settings.
- prebuilt .bbl files: bibliography fallback for submission systems that do not run BibTeX automatically.
- Pictures/: all figures and supporting figure source/data files from the original submission.

COMPILATION
Recommended: set main.tex as the main document and use latexmk/Overleaf's normal Recompile command. The included latexmkrc runs the required LaTeX/BibTeX passes automatically.

Manual fallback:
1. Run pdflatex main.tex.
2. Run bibtex main.
3. Run pdflatex main.tex twice more.

To compile the highlighted version manually, use the same sequence with manuscript_revised_highlighted in place of main.
The source includes self-contained pseudocode commands and does not require the optional algorithm/algorithmic packages.

SCIENTIFIC SCOPE NOTE
The source supplied for revision contains one static benchmark (Carclaims) and no genuinely independent second claims dataset or prediction-level output needed to rerun new methods. The revised manuscript therefore:
- adds the requested state-of-the-art positioning table and recent literature;
- converts the three existing architectures and threshold grid into a formal component ablation and sensitivity analysis;
- explicitly limits claims to within-dataset evidence; and
- specifies the required independent-validation protocol without inventing results.

QUALITY CHECKS COMPLETED
- Clean manuscript compiled successfully: 34 pages.
- Highlighted manuscript compiled successfully: 34 pages; yellow revision markers included.
- Response letter compiled successfully: 2 pages.
- References and cross-references resolved; no undefined-citation or undefined-reference warnings remain.
- No overfull or underfull box warnings remain in either manuscript build log.
- All supplied figures render correctly.

Before uploading, the corresponding author should confirm the author list, affiliations, funding/open-access statements, and any journal-specific source naming requirements in Editorial Manager.
