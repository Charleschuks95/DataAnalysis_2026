# ============================================================================
# Assignment: Distribution fitting by outcome group (including extra points)
# Corrected – no mice required
# ============================================================================

# ----------------------------- 1. Setup -------------------------------------
packages_needed <- c("MASS", "dplyr")
for (pkg in packages_needed) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ----------------------------- 2. Read data ---------------------------------
factor_df <- read.csv("C:\\Users\\Charl\\Documents\\Data Analysis\\Data Manipulation and Cleaning\\Practice 2\\factor_data.csv")
imputed_df <- read.csv("C:\\Users\\Charl\\Documents\\Data Analysis\\Data Manipulation and Cleaning\\Practice 2\\imputed_data.csv")

# ----------------------------- 3. Merge (base R) ----------------------------
data_for_analysis <- merge(imputed_df, factor_df, by = "record_id")
cat("Merged dataset dimensions:", dim(data_for_analysis), "\n")

# ----------------------------- 4. Helper function: best distribution -------
fit_best_dist <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) < 10) return(NULL)
  
  fit_norm <- tryCatch(MASS::fitdistr(x, "normal"), error = function(e) NULL)
  fit_lnorm <- NULL
  if (all(x > 0)) fit_lnorm <- tryCatch(MASS::fitdistr(x, "lognormal"), error = function(e) NULL)
  fit_exp <- NULL
  if (all(x > 0)) fit_exp <- tryCatch(MASS::fitdistr(x, "exponential"), error = function(e) NULL)
  fit_gamma <- NULL
  if (all(x > 0)) fit_gamma <- tryCatch(MASS::fitdistr(x, "gamma"), error = function(e) NULL)
  
  fits <- list()
  if (!is.null(fit_norm)) fits$normal <- fit_norm
  if (!is.null(fit_lnorm)) fits$lognormal <- fit_lnorm
  if (!is.null(fit_exp)) fits$exponential <- fit_exp
  if (!is.null(fit_gamma)) fits$gamma <- fit_gamma
  
  if (length(fits) == 0) return(NULL)
  
  bic_vals <- sapply(names(fits), function(n) BIC(fits[[n]]))
  best <- names(fits)[which.min(bic_vals)]
  best_fit <- fits[[best]]
  
  params <- best_fit$estimate
  param_str <- paste(names(params), "=", round(params, 4), collapse = "; ")
  
  return(list(distribution = best, BIC = min(bic_vals), param_str = param_str))
}

# ----------------------------- 5. Main task (exclude lipids5) --------------
all_num <- names(data_for_analysis)[sapply(data_for_analysis, is.numeric)]
cont_vars <- setdiff(all_num, c("record_id", "outcome"))
cont_vars_main <- setdiff(cont_vars, "lipids5")

results_main <- list()

for (var in cont_vars_main) {
  for (grp in c(0, 1)) {
    vals <- data_for_analysis$outcome == grp
    x <- data_for_analysis[vals, var]
    
    n <- length(x)
    mn <- mean(x, na.rm = TRUE)
    sdev <- sd(x, na.rm = TRUE)
    minv <- min(x, na.rm = TRUE)
    maxv <- max(x, na.rm = TRUE)
    
    best_info <- fit_best_dist(x)
    if (is.null(best_info)) {
      best_dist <- NA; bic_val <- NA; param_str <- NA
    } else {
      best_dist <- best_info$distribution
      bic_val <- best_info$BIC
      param_str <- best_info$param_str
    }
    
    results_main[[length(results_main) + 1]] <- data.frame(
      variable = var,
      outcome_group = grp,
      n = n,
      mean = mn,
      sd = sdev,
      min = minv,
      max = maxv,
      best_distribution = best_dist,
      parameters = param_str,
      BIC = bic_val,
      stringsAsFactors = FALSE
    )
  }
}

results_main_df <- do.call(rbind, results_main)
write.csv(results_main_df, "distribution_results_main.csv", row.names = FALSE)
cat("Main results saved to distribution_results_main.csv\n")

# ----------------------------- 6. Extra points: include lipids5 ------------
cat("\n--- Extra points: including lipids5 ---\n")
# Check if lipids5 has any missing values – if yes, we cannot fix because we don't have mice.
# But from your imputed_data.csv, lipids5 should already be complete. We'll verify.
if (any(is.na(data_for_analysis$lipids5))) {
  cat("WARNING: lipids5 contains NAs. Without mice, we cannot impute.\n")
  cat("Proceeding with available cases only.\n")
  # Remove rows with NA in lipids5 for the extra analysis
  data_extra <- data_for_analysis[!is.na(data_for_analysis$lipids5), ]
} else {
  data_extra <- data_for_analysis
  cat("No missing values in lipids5. Good to go.\n")
}

cont_vars_extra <- c(cont_vars_main, "lipids5")
results_extra <- list()

for (var in cont_vars_extra) {
  for (grp in c(0, 1)) {
    vals <- data_extra$outcome == grp
    x <- data_extra[vals, var]
    
    n <- length(x)
    mn <- mean(x, na.rm = TRUE)
    sdev <- sd(x, na.rm = TRUE)
    minv <- min(x, na.rm = TRUE)
    maxv <- max(x, na.rm = TRUE)
    
    best_info <- fit_best_dist(x)
    if (is.null(best_info)) {
      best_dist <- NA; bic_val <- NA; param_str <- NA
    } else {
      best_dist <- best_info$distribution
      bic_val <- best_info$BIC
      param_str <- best_info$param_str
    }
    
    results_extra[[length(results_extra) + 1]] <- data.frame(
      variable = var,
      outcome_group = grp,
      n = n,
      mean = mn,
      sd = sdev,
      min = minv,
      max = maxv,
      best_distribution = best_dist,
      parameters = param_str,
      BIC = bic_val,
      stringsAsFactors = FALSE
    )
  }
}

results_extra_df <- do.call(rbind, results_extra)
write.csv(results_extra_df, "distribution_results_with_lipids5.csv", row.names = FALSE)
cat("Extra results (including lipids5) saved to distribution_results_with_lipids5.csv\n")

# ----------------------------- 7. Completion -------------------------------
cat("\nAll done!\n")
cat("Main results:", nrow(results_main_df), "rows\n")
cat("Extra results:", nrow(results_extra_df), "rows\n")