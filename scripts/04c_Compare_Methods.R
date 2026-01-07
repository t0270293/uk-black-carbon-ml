# ==============================================================================
# Script: 04c_Compare_Methods.R
# Purpose: Compare the performance of the Stacked Ensemble (ML) against 
#          baseline methods (Linear Regression, IDW) for both UB and RB sites.
# ==============================================================================

# 1. Load Packages
# ------------------------------------------------------------------------------
library(tidyverse)
library(h2o)
library(gstat)   # For IDW
library(sp)      # For spatial coordinates
library(lubridate)

# Initialize H2O
h2o.init(nthreads = -1, max_mem_size = "12G")

# 2. Helper Function: Calculate Baselines
# ------------------------------------------------------------------------------
# This function takes the cleaned data, features list, and site type (UB/RB)
# and returns a performance table for Linear Regression and IDW.
run_baseline_comparison <- function(data_path, features, site_type) {
  
  message(paste("--- Running Baselines for", site_type, "---"))
  
  # Load Data
  full_data <- readRDS(data_path)
  
  # Prepare Train/Test Split (Same logic as 04a/b: 80/20 split on valid BC data)
  valid_bc_data <- full_data %>% filter(!is.na(bc))
  
  set.seed(123)
  train_indices <- sample(seq_len(nrow(valid_bc_data)), size = 0.8 * nrow(valid_bc_data))
  train_df <- valid_bc_data[train_indices, ]
  test_df  <- valid_bc_data[-train_indices, ]
  
  # --- Method 1: Linear Regression ---
  message("Running Linear Regression...")
  # Construct formula: bc ~ feature1 + feature2 ...
  f <- as.formula(paste("bc ~", paste(features, collapse = " + ")))
  
  lm_model <- lm(f, data = train_df)
  lm_pred <- predict(lm_model, newdata = test_df)
  
  lm_perf <- c(
    R2 = cor(test_df$bc, lm_pred, use = "complete.obs")^2,
    MAE = mean(abs(test_df$bc - lm_pred), na.rm = TRUE),
    RMSE = sqrt(mean((test_df$bc - lm_pred)^2, na.rm = TRUE))
  )
  
  # --- Method 2: Inverse Distance Weighting (IDW) ---
  message("Running IDW (Spatial Interpolation)...")
  
  # Prepare spatial objects
  # Note: gstat requires non-missing coordinates
  train_sp <- train_df %>% filter(!is.na(longitude) & !is.na(latitude))
  test_sp  <- test_df  %>% filter(!is.na(longitude) & !is.na(latitude))
  
  coordinates(train_sp) <- ~longitude + latitude
  coordinates(test_sp)  <- ~longitude + latitude
  
  # IDW Model (power = 2 is standard)
  idw_model <- gstat::idw(bc ~ 1, locations = train_sp, newdata = test_sp, idp = 2.0, debug.level = 0)
  idw_pred <- idw_model$var1.pred
  
  idw_perf <- c(
    R2 = cor(test_sp$bc, idw_pred, use = "complete.obs")^2,
    MAE = mean(abs(test_sp$bc - idw_pred), na.rm = TRUE),
    RMSE = sqrt(mean((test_sp$bc - idw_pred)^2, na.rm = TRUE))
  )
  
  # --- Combine Results ---
  baseline_results <- rbind(
    Linear_Regression = lm_perf,
    IDW = idw_perf
  ) %>%
    as.data.frame() %>%
    rownames_to_column("Method") %>%
    mutate(Site_Type = site_type)
  
  return(baseline_results)
}

# 3. Load ML Performance (From 04a/b)
# ------------------------------------------------------------------------------
# We load the CSVs saved in the previous steps to avoid re-training the massive models.
load_ml_perf <- function(path, site_type) {
  if (file.exists(path)) {
    df <- read_csv(path, show_col_types = FALSE) %>%
      mutate(Site_Type = site_type) %>%
      rename(Method = Model) # Standardize column name
    return(df)
  } else {
    warning(paste("ML Performance file not found for", site_type))
    return(NULL)
  }
}

ml_ub <- load_ml_perf("data/processed/04a_UB_model_performance.csv", "UB")
ml_rb <- load_ml_perf("data/processed/04b_RB_model_performance.csv", "RB")

# 4. Run Baselines
# ------------------------------------------------------------------------------

# Define features used in 04a/b
features_ub <- c("no", "no2", "o3", "pm2.5", "pm10", "dec_day", "blh") 
# Note: 'longitude' excluded from LM as it's a spatial proxy, but IDW handles space.
# If you used 'e' or 'slhf' in 04a, add them here.

features_rb <- c("no2", "pm2.5", "o3", "pm10", "blh", "no", "t2m", "d2m")

# Calculate Baselines
if (file.exists("data/processed/01a_UB_data_clean.rds")) {
  # Note: We need the merged file from step 04a. Since 04a didn't save the intermediate 
  # merged file, we will reload 04a_predictions_final.rds which contains all cols.
  base_ub <- run_baseline_comparison("data/processed/04a_UB_predictions_final.rds", features_ub, "UB")
} else {
  base_ub <- NULL
}

if (file.exists("data/processed/04b_RB_predictions_final.rds")) {
  base_rb <- run_baseline_comparison("data/processed/04b_RB_predictions_final.rds", features_rb, "RB")
} else {
  base_rb <- NULL
}

# 5. Combine and Save All Results
# ------------------------------------------------------------------------------
final_comparison <- bind_rows(ml_ub, ml_rb, base_ub, base_rb) %>%
  arrange(Site_Type, desc(R2)) %>%
  dplyr::select(Site_Type, Method, R2, MAE, RMSE)

print(final_comparison)

write_csv(final_comparison, "data/processed/04c_model_comparison_all.csv")

message("Comparison complete. Results saved to 'data/processed/04c_model_comparison_all.csv'")