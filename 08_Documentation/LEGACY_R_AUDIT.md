# Findings from the original R project

The full original thesis directory is preserved byte-for-byte in `07_Original_Archive/Master_Thesis`. This audit concerns observable code behavior. It does not assert that every historical export came from exactly one script version.

| Source evidence | Issue and consequence | Revised treatment |
|---|---|---|
| `PreProcessing.R`, lines 81–93: selected feature count replaces `response_column_number`, followed by `h2o.importFile(path)` and positional column removal | The raw H2O table can lose a predictor instead of the target; its raw row order also differs from shuffled data | Remove target by name once; use one aligned feature matrix |
| `Labaling.R`, lines 249 onward; `LabalingMultiLayer.R`, lines 270 onward: `I1 = which(full_data_label1 == 1)` | True fraud outcomes directly influence initial training selection | Form alert/audit masks without outcomes, then reveal only selected labels |
| `FinalModeling4.R`, lines 67/71 and later: `ovun.sample(..., method='over')` | ROSE random oversampling is not an executed SMOTE call | Genuine training-only SMOTE, with documented small-class fallback |
| `Labaling.R`, lines 65/67; `LabalingMultiLayer.R`, lines 64/66 | Absolute Z threshold is 1.64 rather than the manuscript's 3 | Fixed absolute Z threshold 3 in both manuscript and revised code |
| `EntropyMethod.R`: `mean(apply(Data[-i, ...], 2, entropi))` | Computes remaining-set entropy without subtracting from original entropy; larger remaining entropy is a different ranking | Compute the stated entropy gain exactly |
| `EntropyMethod.R`: two `>= quantile(...)` conditions joined by `&` | The lower-quantile condition is redundant; this is not a two-tailed rule | Explicit top-20% entropy-gain rule |
| `EntropyMethod.R`: `Data = Data[1:100,]` | This standalone exploration uses only 100 rows | Full prescribed D1 working set in revised runs |
| `LabalingMultiLayer.R`, line 232: `total_result[table(total_result) > 1]` | Logical indexing of a vote-occurrence vector is not extraction of record IDs from the table names | Candidate IDs derived directly from row-wise vote totals; entropy runs on those candidates |
| Many `confusionMatrix(..., mode='everything')` calls omit `positive` | Metrics may describe the first factor level, often non-fraud, rather than fraud | Explicit class 1 for all fraud precision/recall/F1 calculations |

The positive-class issue is a reason not to relabel old recall/F1 columns. The new evaluation is recomputed from actual row-level predictions. Old exported results are retained as historical evidence, not spliced into revised tables.

The three labeling files implement flat nine-detector, flat entropy-enhanced and candidate-gated ideas. The multiple `FinalModeling` versions explore learner subsets and vote thresholds. This concept is retained in the modular Python revision, while the final experiment fixes all eight learners and tests two documented threshold pairs. Legacy behavior and package defaults are not claimed to be exactly reproduced.

`fraud_oracle.csv` is not independent: after matching names and normalizing thousands commas, all 32 predictors and all 15,420 outcomes match Carclaims row-for-row. `audit_inputs.py` creates the detailed verification. Insurance Claims differs in schema and size, but the archive does not establish its originating insurer or full data-generation history.
