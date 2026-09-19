# Original workflow images and executed ggplot2 graphics

The final article contains ten figures. Figures 1 and 2 are **byte-identical**
to the submitted V5 images `simple.png` and `Multilayered.png`; editable
`Simple.drawio` and `Multilayered.drawio` sources are supplied. The surrounding
method text explains their conceptual shorthand, including training-only
preprocessing, verified non-alert records and the unused final model update.
These PNGs have not been retouched or regenerated.

Figures 3--10 were rendered by actual R 4.3.3 / ggplot2 3.4.4 execution of
`01_Code/Plot_Article_ggplot2.R`. They use the submitted article's gray
background, facets, three-architecture hue palette, linetypes and symbols.
The additional static ablation has a gray colour/symbol. Both datasets use
one script. PNGs are 720 dpi; PDF, SVG and EPS exports are also supplied.

| Final figure | Asset stem | Source / meaning |
|---|---|---|
| 1 | simple | Original simple-architecture image, unchanged |
| 2 | Multilayered | Original multilayer image, unchanged |
| 3 | 02_architecture_performance | D2--D4 pooled fraud recall, F1, AP; mean and sample SD |
| 4 | 03_partition_progress | Partition-wise fraud F1 and recall; line/point facets |
| 5 | 04_workload | Partition-wise alert/review percentages; original line/point approach |
| 6 | 05_baseline_comparison | F1/AP for same-acquisition classifiers |
| 7 | 06_roc_pr | Saved seed-1234 ROC/PR curves |
| 8 | 07_ablation_deltas | Paired updating-minus-static changes in percentage points |
| 9 | 08_threshold_sensitivity | Two actually executed joint settings, (3,4) and (5,6) |
| 10 | 09_runtime | Three supervised fitting rounds; no shared anomaly/comparator search time |

This restores the **visual language**, not the old numerical curves. The old
full threshold grids in `FIG1.JPEG`, `FIG2.PNG` and related figures have not
been rerun by this presentation revision. They remain in the original archive
and are not relabeled as current evidence. Displaying such a grid would
require corresponding new executions. The revised paper states its actual
two-setting scope.

`build_assets.py` prepares audited plot-data CSVs and numerical LaTeX;
`build_preserved_assets.py` restores the original images and frequency table;
the R script produces all final empirical graphics. Its versions and output
hashes are in `ggplot2_build_report.json`. `ggplot2_verification.json` checks
R means/SDs against Python results and confirms unchanged workflow bytes.

## Final editorial plotting pass

The final plots retain ggplot2's gray panels and existing color/line conventions.
Their nominal width is 6.5 inches, with base text size 12 points and explicit
axis and legend text sizes suitable for the manuscript's narrower text area.
Performance figures use a 0–80% display range and the workload figure 0–55%;
ROC/PR panels retain 0–1 axes. These are display coordinates, not transformations
of observations. The audit checks that the mean ± SD endpoints remain visible.
Ablation and runtime facets have explicitly labeled separate ranges, as
explained in their captions. Figure 10 excludes shared anomaly detection and
comparator tuning; no timing claim beyond the stored run scope is made.

Text, captions and float placement were edited; data, core model sources and
all previously executed numerical result files are unchanged. The final
availability statement links to the public GitHub repository supplied by the
author, also cited in the response letter: https://github.com/Mehrab-Atighi/Anomaly-Detection-Algorithm-for-Enhanced-Fraud-Identification-in-the-Insurance-Sector.
