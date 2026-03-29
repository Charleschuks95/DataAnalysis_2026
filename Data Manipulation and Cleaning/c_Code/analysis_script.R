# ============================================
# COMPLETE ANALYSIS SCRIPT
# Missing Data Analysis & Outlier Detection
# ============================================

# Clear workspace
rm(list = ls())

# ========== 1. SETUP AND DATA LOADING ==========

# Set working directory (modify as needed)
# setwd("path/to/your/project/folder")

# Load data
data_path <- "b_Data/DataSet_No_Details.csv"
df <- read.csv(data_path)

cat("Data loaded. Dimensions:", dim(df), "\n")

# Install and load required packages
packages <- c("dplyr", "naniar", "mice", "ggplot2", 
              "tidyr", "dbscan", "skimr", "visdat")

for(pkg in packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ========== 2. DATA PREPARATION ==========

# Remove specified columns
cols_to_remove <- c("h_index_34", "h_index_56", "hormone10_1", "hormone10_2",
                    "an_index_23", "outcome", "factor_eth", "factor_h", 
                    "factor_pcos", "factor_prl")
MD_df <- df %>% select(-any_of(cols_to_remove))

# Remove columns with high missingness (>35%)
cols_to_remove1 <- c("hormone9", "hormone11", "hormone12", "hormone13", "hormone14")
handle_MD_df <- MD_df %>% select(-any_of(cols_to_remove1))

cat("Cleaned dataset dimensions:", dim(handle_MD_df), "\n")

# ========== 3. TASK 1: LITTLE'S MCAR TEST ==========

cat("\n========== TASK 1: MCAR TEST ==========\n")
mcar_result <- mcar_test(handle_MD_df)
print(mcar_result)

# Save results as CSV
write.csv(data.frame(mcar_result), "d_Outputs/Task1_MCAR_Results.csv", row.names = FALSE)

# ========== 4. TASK 2: IMPUTATION COMPARISON ==========

cat("\n========== TASK 2: IMPUTATION COMPARISON ==========\n")

# PMM Imputation
cat("Running PMM imputation...\n")
imputed_pmm <- mice(handle_MD_df, m = 5, method = 'pmm', printFlag = FALSE, seed = 123)
imputed_pmm_final <- complete(imputed_pmm)

# RF Imputation
cat("Running RF imputation...\n")
imputed_rf <- mice(handle_MD_df, m = 5, method = 'rf', printFlag = FALSE, seed = 123)
imputed_rf_final <- complete(imputed_rf)

# Compare distributions for hormone10_generated
if("hormone10_generated" %in% names(handle_MD_df)) {
  
  original_clean <- handle_MD_df$hormone10_generated[!is.na(handle_MD_df$hormone10_generated)]
  
  comparison_df <- data.frame(
    value = c(original_clean,
              imputed_pmm_final$hormone10_generated,
              imputed_rf_final$hormone10_generated),
    type = c(rep("Original", length(original_clean)),
             rep("PMM Imputed", nrow(imputed_pmm_final)),
             rep("RF Imputed", nrow(imputed_rf_final)))
  )
  
  # Density plot
  p_density <- ggplot(comparison_df, aes(x = value, fill = type)) +
    geom_density(alpha = 0.5) +
    labs(title = "Comparison of Imputation Methods: PMM vs RF",
         subtitle = "Variable: hormone10_generated",
         x = "Value", y = "Density") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave("d_Outputs/Task2_Density_Plot.png", p_density, width = 8, height = 6)
  print(p_density)
  
  # Summary statistics
  summary_stats <- data.frame(
    Method = c("Original", "PMM Imputed", "RF Imputed"),
    Mean = c(mean(original_clean, na.rm = TRUE),
             mean(imputed_pmm_final$hormone10_generated),
             mean(imputed_rf_final$hormone10_generated)),
    Median = c(median(original_clean, na.rm = TRUE),
               median(imputed_pmm_final$hormone10_generated),
               median(imputed_rf_final$hormone10_generated)),
    SD = c(sd(original_clean, na.rm = TRUE),
           sd(imputed_pmm_final$hormone10_generated),
           sd(imputed_rf_final$hormone10_generated))
  )
  
  write.csv(summary_stats, "d_Outputs/Task2_Summary_Stats.csv", row.names = FALSE)
  print(summary_stats)
}

# ========== 5. TASK 3: LOF OUTLIER DETECTION ==========

cat("\n========== TASK 3: LOF OUTLIER DETECTION ==========\n")

# Use PMM imputed dataset
outlier_data <- imputed_pmm_final

# Select numeric columns
numeric_cols <- outlier_data %>% select(where(is.numeric))

# Remove record_id
if("record_id" %in% names(numeric_cols)) {
  numeric_cols <- numeric_cols %>% select(-record_id)
}

# Remove rows with NAs
if(sum(is.na(numeric_cols)) > 0) {
  numeric_cols <- na.omit(numeric_cols)
}

# Scale data
scaled_data <- scale(numeric_cols)

# Calculate LOF scores
min_pts <- 5
lof_scores <- lof(scaled_data, minPts = min_pts)

# Create results
result_df <- data.frame(
  original_row = 1:nrow(scaled_data),
  lof_score = lof_scores,
  is_outlier = lof_scores > 1.5
)

# Save LOF results
write.csv(result_df, "d_Outputs/Task3_LOF_Results.csv", row.names = FALSE)

# Histogram
p_hist <- ggplot(result_df, aes(x = lof_score)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "black") +
  geom_vline(xintercept = 1.5, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Local Outlier Factor (LOF) Scores",
       subtitle = paste("minPts =", min_pts, "| Total observations:", nrow(result_df)),
       x = "LOF Score", y = "Frequency") +
  theme_minimal()

ggsave("d_Outputs/Task3_LOF_Histogram.png", p_hist, width = 8, height = 6)

# Scatterplot
if(ncol(numeric_cols) >= 2) {
  plot_df <- data.frame(
    var1 = numeric_cols[[1]],
    var2 = numeric_cols[[2]],
    is_outlier = result_df$is_outlier
  )
  
  p_scatter <- ggplot(plot_df, aes(x = var1, y = var2, color = is_outlier)) +
    geom_point(alpha = 0.6, size = 2) +
    scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red")) +
    labs(title = "Bivariate Scatterplot with LOF Outliers",
         x = names(numeric_cols)[1], y = names(numeric_cols)[2]) +
    theme_minimal()
  
  ggsave("d_Outputs/Task3_Scatterplot.png", p_scatter, width = 8, height = 6)
}

# Boxplot
p_box <- ggplot(result_df, aes(x = is_outlier, y = lof_score, fill = is_outlier)) +
  geom_boxplot() +
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "red")) +
  labs(title = "LOF Score Distribution: Normal vs Outliers",
       x = "Classification", y = "LOF Score") +
  theme_minimal()

ggsave("d_Outputs/Task3_Boxplot.png", p_box, width = 6, height = 5)

# Print summary
cat("\n========== RESULTS SUMMARY ==========\n")
cat("Total observations analyzed:", nrow(result_df), "\n")
cat("Outliers detected (LOF > 1.5):", sum(result_df$is_outlier), "\n")
cat("Percentage of outliers:", round(sum(result_df$is_outlier)/nrow(result_df)*100, 2), "%\n")

cat("\n✅ Analysis Complete! Check the 'd_Outputs' folder for results.\n")