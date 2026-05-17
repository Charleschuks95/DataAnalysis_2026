\# Task 5 – Multivariate Analysis of Arctic Chrysophyte Abundance



\## Description

This task performs multivariate analysis on a dataset of 34 chrysophyte (algae) species from 6 Arctic and subarctic sites in Russia. The analysis includes:



\- NMDS ordination using Bray‑Curtis distance

\- envfit to identify significant species

\- UPGMA hierarchical clustering (Bray‑Curtis)

\- Visualisation of clusters, confidence ellipses, and significant species vectors

\- PERMANOVA to test for differences in species composition between clusters



\## Data

\- \*\*File:\*\* `data.txt`

\- \*\*Format:\*\* Tab‑separated, first column = site names, next 34 columns = species abundance scores (0 = absent, 1 = rare, 2 = moderate, 3 = abundant)

\- \*\*Sites:\*\* 6 (Ledyanaya Gora, Karaul village, Ladyginskie Yary, Sopochnaya Karga, Sibiryakov Island, Chernyi Bay)



\## Requirements

R packages: `vegan`



\## Key Outputs

\- NMDS stress = 0 (warning due to small sample size)

\- No species were significant (envfit p > 0.05)

\- UPGMA produced 2 clusters:

&#x20; - \*\*Cluster 1:\*\* Ledyanaya Gora, Ladyginskie Yary, Sopochnaya Karga, Chernyi Bay

&#x20; - \*\*Cluster 2:\*\* Karaul village, Sibiryakov Island

\- PERMANOVA result: R² = 0.4088, p = 0.0667 (not significant at α = 0.05)



\## Files

\- `Task5\_analysis.R` – complete R script

\- `NMDS\_plot.png` – ordination plot (to be saved)

\- `README.md` – this file



\## How to Run

1\. Place `data.txt` and the R script in the same folder.

2\. Run `Task5\_analysis.R` in RStudio.

3\. The NMDS plot will be displayed (save manually). Console shows PERMANOVA results and cluster membership.



\## Limitations

\- Small sample size (n = 6) leads to zero stress and unstable ordination. Results should be interpreted with caution.



\## Author

\[Your Name]



\## Date

2026-05-17

