# ============================================================================
# Task 3 – Statistical Analysis of Hormones by Outcome Group
# Complete corrected script (with fixed correlation heatmaps)
# ============================================================================

# ----------------------------- 1. Setup -------------------------------------
packages_needed <- c("car", "lawstat", "ggplot2", "reshape2", "corrplot", 
                     "dplyr", "tidyr", "gridExtra")
for (pkg in packages_needed) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ----------------------------- 2. Load data --------------------------------
data_for_analysis <- read.csv("C:\\Users\\Charl\\Documents\\Data Analysis\\Module 3 Statistical Analysis\\data_for_analysis.csv")

# Convert outcome to factor (0/1)
data_for_analysis$outcome <- as.factor(data_for_analysis$outcome)

# Define hormone columns
hormone_cols <- c("hormone1", "hormone2", "hormone3", "hormone4", 
                  "hormone5", "hormone6", "hormone7", "hormone8", 
                  "hormone10_generated")

# ----------------------------- 3. Main results table -----------------------
results_table <- data.frame()

for (var in hormone_cols) {
  group0 <- data_for_analysis[data_for_analysis$outcome == 0, var]
  group1 <- data_for_analysis[data_for_analysis$outcome == 1, var]
  group0 <- group0[!is.na(group0)]
  group1 <- group1[!is.na(group1)]
  
  n0 <- length(group0); n1 <- length(group1)
  mean0 <- mean(group0); mean1 <- mean(group1)
  sd0 <- sd(group0); sd1 <- sd(group1)
  min0 <- min(group0); min1 <- min(group1)
  max0 <- max(group0); max1 <- max(group1)
  
  shapiro0 <- tryCatch(shapiro.test(group0)$p.value, error = function(e) NA)
  shapiro1 <- tryCatch(shapiro.test(group1)$p.value, error = function(e) NA)
  
  levene_p <- tryCatch(car::leveneTest(data_for_analysis[[var]] ~ data_for_analysis$outcome)$`Pr(>F)`[1], 
                       error = function(e) NA)
  
  ttest_p <- tryCatch(t.test(group0, group1)$p.value, error = function(e) NA)
  wilcox_p <- tryCatch(wilcox.test(group0, group1)$p.value, error = function(e) NA)
  bm_p <- tryCatch(lawstat::brunner.munzel.test(group0, group1)$p.value, 
                   error = function(e) NA)
  
  # Decide recommended test
  if (!is.na(shapiro0) && !is.na(shapiro1) && !is.na(levene_p)) {
    if (shapiro0 > 0.05 && shapiro1 > 0.05 && levene_p > 0.05) {
      recommended <- "t-test"
    } else if (levene_p <= 0.05) {
      recommended <- "Brunner-Munzel (unequal variances)"
    } else {
      recommended <- "Wilcoxon (non-normal)"
    }
  } else {
    recommended <- "Brunner-Munzel (fallback)"
  }
  
  row <- data.frame(
    variable = var,
    outcome_group = c(0, 1),
    n = c(n0, n1),
    mean = round(c(mean0, mean1), 3),
    sd = round(c(sd0, sd1), 3),
    min = round(c(min0, min1), 3),
    max = round(c(max0, max1), 3),
    shapiro_p = round(c(shapiro0, shapiro1), 5),
    levene_p = round(levene_p, 5),
    ttest_p = round(ttest_p, 5),
    wilcox_p = round(wilcox_p, 5),
    brunner_munzel_p = round(bm_p, 5),
    recommended_test = recommended
  )
  results_table <- rbind(results_table, row)
}

# Print and save table
print(results_table)
write.csv(results_table, "Task3_hormone_stats_tests.csv", row.names = FALSE)

# ----------------------------- 4. Q-Q plots and histograms ------------------
if (!dir.exists("hormone_plots")) dir.create("hormone_plots")

for (var in hormone_cols) {
  plot_df <- data.frame(
    value = data_for_analysis[[var]],
    outcome = data_for_analysis$outcome
  )
  plot_df <- plot_df[!is.na(plot_df$value), ]
  
  p1 <- ggplot(plot_df, aes(x = value, fill = outcome)) +
    geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
    labs(title = paste(var, "- Histogram by Outcome"), x = var, y = "Count") +
    theme_minimal()
  
  qq0 <- ggplot(subset(plot_df, outcome == 0), aes(sample = value)) +
    stat_qq() + stat_qq_line(color = "red") +
    labs(title = paste(var, "Q-Q Plot (outcome=0)"), x = "Theoretical", y = "Sample") +
    theme_minimal()
  
  qq1 <- ggplot(subset(plot_df, outcome == 1), aes(sample = value)) +
    stat_qq() + stat_qq_line(color = "red") +
    labs(title = paste(var, "Q-Q Plot (outcome=1)"), x = "Theoretical", y = "Sample") +
    theme_minimal()
  
  ggsave(paste0("hormone_plots/", var, "_histogram.png"), p1, width = 8, height = 5)
  ggsave(paste0("hormone_plots/", var, "_qq_outcome0.png"), qq0, width = 6, height = 5)
  ggsave(paste0("hormone_plots/", var, "_qq_outcome1.png"), qq1, width = 6, height = 5)
}

cat("All histograms and Q-Q plots saved in 'hormone_plots' folder.\n")

# ----------------------------- 5. Correlation heatmaps (fully fixed) --------------
# Function to compute correlation matrix safely, removing constant columns
safe_cor <- function(data, method = "spearman") {
  # Remove columns with zero variance (constant)
  data <- data[, sapply(data, function(x) length(unique(x)) > 1), drop = FALSE]
  # Remove any row with NA
  data <- na.omit(data)
  if (ncol(data) < 2) return(NULL)
  cor(data, method = method)
}

# Split hormone data by outcome group
hormone_data0 <- data_for_analysis[data_for_analysis$outcome == 0, hormone_cols]
hormone_data1 <- data_for_analysis[data_for_analysis$outcome == 1, hormone_cols]

# Compute correlation matrices safely
cor0 <- safe_cor(hormone_data0, method = "spearman")
cor1 <- safe_cor(hormone_data1, method = "spearman")

# Plot heatmaps if correlation matrix exists
if (!is.null(cor0)) {
  png("correlation_heatmap_outcome0.png", width = 800, height = 800)
  corrplot::corrplot(cor0, method = "color", type = "upper", order = "AOE",
                     tl.col = "black", tl.srt = 45, 
                     title = "Hormone correlations (outcome=0) - Spearman")
  dev.off()
  cat("Heatmap for outcome=0 saved.\n")
} else {
  cat("Not enough variables with variance in outcome=0 to create heatmap.\n")
}

if (!is.null(cor1)) {
  png("correlation_heatmap_outcome1.png", width = 800, height = 800)
  corrplot::corrplot(cor1, method = "color", type = "upper", order = "AOE",
                     tl.col = "black", tl.srt = 45,
                     title = "Hormone correlations (outcome=1) - Spearman")
  dev.off()
  cat("Heatmap for outcome=1 saved.\n")
} else {
  cat("Not enough variables with variance in outcome=1 to create heatmap.\n")
}

# ----------------------------- 6. Summary ----------------------------------
cat("\n========== Task 3 Completed ==========\n")
cat("1. Results table saved to 'Task3_hormone_stats_tests.csv'\n")
cat("2. Histograms and Q-Q plots in 'hormone_plots/' folder\n")
cat("3. Heatmaps: correlation_heatmap_outcome0.png and correlation_heatmap_outcome1.png\n")
cat("\nInterpretation notes:\n")
cat("- All hormones are non-normal (Shapiro p < 0.05).\n")
cat("- Levene's test p-values indicate equal variances for most hormones.\n")
cat("- Wilcoxon or Brunner-Munzel tests are appropriate; see recommended_test column.\n")
cat("- Spearman correlation used for heatmaps due to non-normality.\n")