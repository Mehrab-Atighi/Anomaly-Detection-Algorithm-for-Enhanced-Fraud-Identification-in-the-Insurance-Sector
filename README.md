# SAD revision in the submitted V5 format

This is a revision within the original article. The supplied final V5 archive
confirms the original source, page geometry, compact title block, five main
sections, methodology order, three formal algorithms and structural images.
Figures 1 and 2 use the original `simple.png` and `Multilayered.png` bytes.
Their conceptual shorthand is explained in the adjacent implementation note.

The model code, input data and executed results are carried forward unchanged
from the audited Python package. No new model fitting was performed for this
presentation revision. All eight empirical figures have now been **executed
in R/ggplot2**, using the previous article's gray panels, facets, hue colours,
line types and point symbols. The same R script draws both datasets.
The old exhaustive threshold grid is not represented as rerun: current
sensitivity figures report only the two actually executed joint settings.

## Files to use

- `05_Manuscript/manuscript_clean.pdf`: reading copy; `.tex` is editable.
- `05_Manuscript/manuscript_marked.pdf` and `.tex`: substantive changes in blue.
- `00_JOURNAL_SOURCE_ONLY.zip`: complete editable-source submission, including
  clean/marked sources, 40 references, figure assets and reviewer response.
  **No PDFs are included in this inner archive**, as instructed by the editor.
- `06_Reviewer_and_Professor`: point-by-point response, change map, Persian
  professor report and textual source comparison.
- `01_Code/sad`: executed Python model modules; `03_Results`: predictions,
  per-seed metrics, timings and manifests; `02_Data`: the two input CSVs.
- `01_Code/Plot_Article_ggplot2.R`: final figure code; `04_Figures/plot_data`:
  values; `04_Figures`: eight empirical figures in PNG/PDF/SVG/EPS, plus the
  two untouched structural PNGs and their editable draw.io sources.
- `07_Original_Archive`: original R thesis sources/outputs and article files.
  `Submitted_V5_Confirmed` is the latest supplied final-submission archive.

## Reproduction

Install Python 3.12, R and the listed packages. The delivered figures were
rendered with R 4.3.3 and ggplot2 3.4.4; complete session details are in
`08_Documentation/R_ggplot2_sessionInfo.txt`.

```bash
python -m pip install -r requirements.txt
Rscript 01_Code/Install_Plot_Packages.R
python RUN_ALL.py --rebuild-only
```

The workflow verifies the saved results, prepares plot-data CSVs and numerical
LaTeX, restores the original workflow files, executes R/ggplot2, and verifies
the R summaries against Python. Add `--pdf` to build the five reading documents
and the source ZIP (pdfLaTeX, BibTeX and XeLaTeX required).

```bash
python RUN_ALL.py --rebuild-only --pdf
python RUN_ALL.py --rerun --pdf
```

`--rerun` writes new model runs to `03_Results_reproduced`. The distributed
`03_Results` remains the release's evidence. Individual steps can be run:

```bash
python 01_Code/build_assets.py
python 01_Code/build_preserved_assets.py
Rscript 01_Code/Plot_Article_ggplot2.R
python 01_Code/verify_ggplot_outputs.py
```

`build_assets.py` prepares numerical assets; optional `--python-preview`
renders intermediate Python diagnostics, which are not the final article
style. Re-execute the R script afterwards to restore final graphics.
`RUN_FROM_R.R` is an optional launcher, not a reimplementation of the models.

## Interpretation and release checks

The second file tests replication on another benchmark schema; its original
insurer provenance is unverified. SAD is not claimed to outperform every
comparator. The PSO-XGBoost comparator is an explicitly adapted implementation.
An AI-assistance statement describes help with code and text; the authors
remain responsible for reviewing and approving the science and final text.

`REPRODUCIBILITY.md`, numerical verification, the R session record and the
cross-language plot audit are in `08_Documentation`. The release manifest
records every distributed file's SHA256. Run
`python 01_Code/verify_package.py` to verify an extracted archive. Regenerated
or edited files will correctly be reported as changed.

## Final editorial pass

The second dataset is cited to the researcher-confirmed
[Insurance Claims / mexwell source](https://www.kaggle.com/datasets/mexwell/insurance-claims).
Names remain in the final article; no anonymous version is included.
Section 4.6 and the response letter link to the author-provided public
[GitHub repository](https://github.com/Mehrab-Atighi/Anomaly-Detection-Algorithm-for-Enhanced-Fraud-Identification-in-the-Insurance-Sector) containing code, data, outputs and reproduction materials.

The user-supplied 14% similarity report and the editorial review record are
in `08_Documentation/similarity` and `FINAL_EDITORIAL_REVIEW.md`. That score
belongs to the version originally checked; no score is asserted for the
final edited text. Language editing was performed directly, without a
Grammarly service run. Figure labels, axis ranges and pagination were improved;
input data, executed results and core model code were not changed.
