# ============================================================================
# Task 5 – Multivariate Analysis of Arctic Chrysophyte Abundance
# Bray–Curtis NMDS, clustering, envfit, PERMANOVA
# ============================================================================

# ----------------------------- 1. Load packages -----------------------------
if (!require(vegan)) install.packages("vegan")
library(vegan)

# ----------------------------- 2. Read data --------------------------------
# Data file: tab‑separated, first column = site names, rest = species (integer 0–3)
data <- read.table("data.txt", header = TRUE, sep = "\t", row.names = 1, check.names = FALSE)

# Check structure
str(data)
summary(data)

# ----------------------------- 3. NMDS with Bray–Curtis --------------------
set.seed(123)  # for reproducibility
ord_bray <- metaMDS(data, distance = "bray", trymax = 50, autotransform = FALSE)
ord_bray$stress   # stress value (should be low, but with n=6 it may be near 0)

# ----------------------------- 4. envfit: significant species --------------
# Fit species vectors onto the NMDS ordination
fit_sp <- envfit(ord_bray, data, permutations = 999)
# Extract species with p ≤ 0.05
sig_species <- which(fit_sp$vectors$p.val <= 0.05)
cat("Significant species (p ≤ 0.05):\n")
print(names(sig_species))

# ----------------------------- 5. UPGMA clustering (Bray–Curtis) ----------
d_bray <- vegdist(data, method = "bray")
hc_bray <- hclust(d_bray, method = "average")

# ----------------------------- 6. Cut dendrogram into 2 clusters ----------
# (You can also try 3 by changing k = 3)
k <- 2
clusters <- cutree(hc_bray, k = k)
clusters <- factor(clusters, labels = c("Cluster 1", "Cluster 2"))

# ----------------------------- 7. Plot NMDS with clusters ------------------
# Define colours for clusters
cols <- c("blue", "red")[as.numeric(clusters)]

# Create empty plot
plot(ord_bray, type = "n", main = paste("NMDS (Bray–Curtis), stress =", round(ord_bray$stress, 4)),
     sub = paste("UPGMA clustering, k =", k))

# Add site points (coloured by cluster)
points(ord_bray, col = cols, pch = 16, cex = 2.5)

# Add 95% confidence ellipses around clusters
ordiellipse(ord_bray, groups = clusters, col = c("blue", "red"), 
            kind = "sd", conf = 0.95, lwd = 2)

# Add site labels without overlap
orditorp(ord_bray, display = "sites", cex = 0.8, col = "black", air = 0.5)

# Add significant species vectors (only those with p ≤ 0.05)
plot(fit_sp, p.max = 0.05, col = "darkgreen", cex = 0.9, add = TRUE)

# Legend
legend("topright", legend = levels(clusters), col = c("blue", "red"), 
       pch = 16, title = "Cluster", bty = "n")

# ----------------------------- 8. PERMANOVA (adonis2) ----------------------
# Test whether clusters differ significantly in species composition
set.seed(456)
adonis_result <- adonis2(data ~ clusters, method = "bray", permutations = 999)

# Print results
print(adonis_result)

# Extract R² and p‑value
R2 <- adonis_result$R2[1]
p_val <- adonis_result$`Pr(>F)`[1]

cat("\n========================================\n")
cat("PERMANOVA (Bray–Curtis) result:\n")
cat("R² =", round(R2, 4), "\n")
cat("p-value =", p_val, "\n")
if (p_val < 0.05) {
  cat("Conclusion: Clusters differ significantly in species composition (p < 0.05).\n")
} else {
  cat("Conclusion: No significant difference between clusters (p ≥ 0.05).\n")
}
cat("========================================\n")

# Optional: Print which sites belong to each cluster
cat("\nCluster membership:\n")
for (i in 1:k) {
  cat(levels(clusters)[i], ":", names(clusters[clusters == levels(clusters)[i]]), "\n")
}