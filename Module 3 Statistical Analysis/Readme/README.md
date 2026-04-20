# Task 3 – Statistical Analysis of Hormones by Outcome Group

## Description
This analysis examines hormone levels (`hormone1` to `hormone8` and `hormone10_generated`) in patients with (outcome=1) and without (outcome=0) tumour. The following steps were performed:

- Descriptive statistics (mean, sd, min, max) by outcome group.
- Shapiro–Wilk normality test and Levene’s test for homogeneity of variance.
- Q‑Q plots and histograms for each hormone by group.
- Group comparisons using t‑test, Wilcoxon test, and Brunner‑Munzel test.
- Spearman correlation heatmaps (due to non‑normality) for each outcome group.

## Data Source
- `data_for_analysis.csv` – merged dataset from previous practicals (1148 rows, 31 columns).

## Output Files

| File | Description |
|------|-------------|
| `Task3_hormone_stats_tests.csv` | Main results table: descriptive stats, p‑values (Shapiro, Levene, t‑test, Wilcoxon, Brunner‑Munzel), and recommended test. |
| `hormone_plots/` | Folder containing histograms and Q‑Q plots for each hormone (27 images). |
| `correlation_heatmap_outcome0.png` | Spearman correlation heatmap for non‑tumour group (outcome=0). |
| `correlation_heatmap_outcome1.png` | Spearman correlation heatmap for tumour group (outcome=1). |

## Key Findings
- All hormones are non‑normal (Shapiro p < 0.05).
- Variances are equal across groups (Levene’s p > 0.05).
- Significant differences between outcome groups were found for `hormone2`, `hormone5`, and `hormone8` (Wilcoxon/Brunner‑Munzel p < 0.01).
- Spearman correlation reveals strong positive relationships (e.g., `hormone2`–`hormone5`, `hormone3`–`hormone4`) in both groups.

## Requirements
R packages: `car`, `lawstat`, `ggplot2`, `corrplot`, `dplyr`, `tidyr`, `gridExtra`.

## How to Reproduce
1. Place `data_for_analysis.csv` in the working directory.
2. Run the R script `Task3_analysis.R` (provided separately).
3. Output files will be generated in the same folder.

## Author
Charles Chukwuemeka

## Date
2026-04-20