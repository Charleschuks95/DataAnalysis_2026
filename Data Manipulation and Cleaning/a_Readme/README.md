# Missing Data Analysis & Outlier Detection

## Assignment Description
Analysis of missing data patterns, multiple imputation (PMM vs RF), and Local Outlier Factor (LOF) outlier detection on medical/biological dataset.

## Data
- **File:** DataSet_No_Details.csv
- **Rows:** 1,148
- **Columns:** 41 (hormones, lipids, antioxidants, metabolic markers)

## R Version
4.5.2

## Procedures Used
1. **Missing Data Analysis:** Little's MCAR Test (`naniar` package)
2. **Multiple Imputation:** PMM (Predictive Mean Matching) vs RF (Random Forest) (`mice` package)
3. **Outlier Detection:** Local Outlier Factor (LOF) algorithm (`dbscan` package)

## Key Results
| Task              | Finding |
| MCAR Test         | p = 0.000 → Data is NOT MCAR |
| Best Imputation   | PMM (better preserves median and SD) |
| Outliers Detected | 9 out of 872 (1.03%) |

## Folder Structure
├── a_Readme/          # This README file
├── b_Data/            # Original dataset
├── c_Code/            # R analysis script
└── d_Outputs/         # Results: plots, CSV files, report
```

## How to Run
1. Open `c_Code/analysis_script.R` in RStudio
2. Run the script section by section
3. Results appear in console and `d_Outputs/` folder

## Required Packages

library(dplyr)
library(naniar)
library(mice)
library(ggplot2)
library(dbscan)


## Author
Charles Chukwuemeka Ndukwe

## Date
2026-03-23
