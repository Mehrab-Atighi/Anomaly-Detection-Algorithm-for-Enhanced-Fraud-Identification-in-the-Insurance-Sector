# Input data provenance and scope

The analysis uses CSV bytes supplied by the researcher in `Master thesis.rar`. Duplicate later uploads have identical archive hashes; see `input_archives.json`. No new online download was substituted for the supplied data.

| Distributed filename | Original filename | Rows | Predictors | Target | Fraud |
|---|---|---:|---:|---|---:|
| `02_Data/carclaims.csv` | `carclaims.csv` | 15,420 | 32 | `FraudFound`, Yes/No | 923 |
| `02_Data/insurance_claims.csv` | `insurance fraud claims.csv` | 1,000 | 38 | `fraud_reported`, Yes/No | 247 |

`Carclaims_data_dictionary.csv` and `InsuranceClaims_data_dictionary.csv` list every raw field, type, unique-count and missing-count after treating `?` as missing. Each seed's fitted predictor selection is recorded separately under `03_Results/<dataset>/<seed>/preprocessing.json`.

The archive's `fraud_oracle.csv` is excluded from the two-benchmark experiment. `audit_inputs.py` verifies all 15,420 outcomes match Carclaims, 31 of 32 predictors match exactly after aligning renamed columns, and the remaining `VehiclePrice` matches after removing thousands commas. Thus it is not evidence from an independent dataset.

The second benchmark has a different schema, sample size and recorded fraud prevalence. This supports replication of the workflow with newly fitted preprocessing/models. It does not prove independent insurer provenance, prospectively collected outcomes, or a genuinely unseen benchmark: the source data had been explored previously.

The bibliography links to [the Carclaims hosting page](https://www.kaggle.com/datasets/antopravinjohnbosco/auto-insurance-claims-fraud-detection). The researcher explicitly identified [Insurance Claims, hosted by mexwell on Kaggle](https://www.kaggle.com/datasets/mexwell/insurance-claims), as the source of the second dataset. That source is cited in the manuscript and bibliography, with an access date of 19 September 2026. The local archival CSV was retained unchanged; a freshly downloaded online file was not substituted or asserted to be byte-identical. The archival files' original collecting entity and complete real/synthetic generation history were not established. No new license or provenance certification is conferred by including these researcher-supplied files.
