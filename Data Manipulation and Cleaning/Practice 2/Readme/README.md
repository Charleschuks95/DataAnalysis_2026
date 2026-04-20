# Distribution Fitting by Outcome Group – Assignment

## Description
For the merged dataset `data_for_analysis` (imputed numeric data + factor data), this assignment fits Normal, Log‑normal, Exponential, and Gamma distributions to each continuous variable, separately for `outcome = 0` (no tumour) and `outcome = 1` (tumour). The best distribution is selected by the lowest Bayesian Information Criterion (BIC). Descriptive statistics and distribution parameters are reported.

## Files
- `distribution_results_main.csv` – Results for all continuous variables **except** `lipids5` (main task).
- `distribution_results_with_lipids5.csv` – Results **including** `lipids5` (extra points).
- `distribution_analysis_script.R` – R script that performs the merging, fitting, and exports the CSV files.

## Data sources (from previous practical)
- `imputed_data.csv` – 26 numeric variables after PMM imputation.
- `factor_data.csv` – `record_id`, `outcome`, and factor variables.

## Methods
- Merging: `merge(imputed_df, factor_df, by = "record_id")`
- Distribution fitting: `MASS::fitdistr` (maximum likelihood)
- Model selection: BIC (lower is better)
- Handling of missing `lipids5`: available‑case analysis (rows with NA excluded)

## How to reproduce
1. Place `imputed_data.csv` and `factor_data.csv` in the working directory.
2. Run the R script `distribution_analysis_script.R`.
3. The two CSV result files will be created in the same folder.

## Requirements
R packages: `MASS`, `dplyr` (install if missing).

## Author
Charles Chukwuemeka Ndukwe

## Date
2026-04-20