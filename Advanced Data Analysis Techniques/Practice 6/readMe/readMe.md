\# PCA of Morphometric Data



\## Description

PCA and correlation analysis of morphometric measurements with grouping factor.



\## Data

\- \*\*File:\*\* `data\_morphometry.txt`

\- \*\*Format:\*\* Tab-separated, first column = group, 11 numeric traits



\## Requirements

```r

install.packages(c("vegan", "factoextra", "plotly"))

```



\## Key Results

| Component | Variance Explained |

|-----------|-------------------|

| PC1 | 51.0% |

| PC2 | 17.2% |

| PC3 | 10.3% |

| \*\*Total (PC1-PC3)\*\* | \*\*78.5%\*\* |



\- \*\*PC1 = Overall size\*\* (all traits load positively)

\- Groups show partial separation along PC1

\- Strong positive correlations among leaf and floral traits



\## Output Files

| File | Description |

|------|-------------|

| `practice 6 PCA\_HW\_eng.R` | R analysis script |

| `Screenshot\_biplot1.png` | PCA biplot (groups, no ellipses) |

| `Screenshot\_biplot2.png` | PCA biplot with 95% ellipses |

| `Screenshot\_3d.png` | Interactive 3D PCA plot |



\## How to Run

1\. Place `data\_morphometry.txt` and R script in same folder

2\. Run script in RStudio (Ctrl+A, Ctrl+Enter)

3\. Save plots from Plots pane and 3D viewer

