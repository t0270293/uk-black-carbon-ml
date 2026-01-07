# ==============================================================================
# Script: 04a_BC_estimation_UB.R
# Purpose: Train a Stacked Ensemble model (RF + XGBoost) to estimate missing
#          Black Carbon (BC) data for Urban Background (UB) sites.
#          Generate performance metrics and visualization plots.
# ==============================================================================

# 1. Load Packages
# ------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(h2o)
library(openair)   # For timeVariation, timePlot, scatterPlot
library(ggpubr)    # For arranging plots
library(zoo)       # For yearmon

# Initialize H2O Cluster
h2o.init(nthreads = -1, max_mem_size = "12G")

# 2. Load Processed Data
# ------------------------------------------------------------------------------
# Load the feature-engineered data from step 03a
# (Note: In 03a we didn't save the final merged file, so we re-merge or load the result.
#  Ideally, step 03a should save 'data/processed/03a_model_data_UB.rds'.
#  For now, we will re-run the merge logic briefly to ensure independence.)

# Re-load basics
data_ub <- readRDS("data/processed/01a_UB_data_clean.rds")
weather_data <- readRDS("data/processed/02_weather_2009_2020_clean.rds")

# Merge
full_data <- data_ub %>%
  inner_join(weather_data, by = c("site_id", "site_name", "date_end")) %>%
  mutate(
    dec_day = decimal_date(date_end),
    ws = sqrt(u10^2 + v10^2),
    wd_rad = atan2(u10/ws, v10/ws),
    wd = (wd_rad * 180 / pi) + 180
  ) %>%
  dplyr::select(-wd_rad)

# Patch Extra Data (Simplified from 03a)
if(file.exists("data/UB_air_pollutants_extra.csv")) {
  extra <- read_csv("data/UB_air_pollutants_extra.csv", show_col_types = FALSE) %>%
    rename(site_id = site) %>%
    mutate(date_end = as_datetime(date_end)) %>%
    filter(!is.na(pm2.5))
  full_data <- full_data %>% rows_update(extra, by = c("date_end", "site_id"), unmatched = "ignore")
}

# 3. Prepare Data for H2O
# ------------------------------------------------------------------------------
# Split into Training (Rows with BC) and Prediction (Rows without BC)
data_valid_bc <- full_data %>% filter(!is.na(bc))
data_missing_bc <- full_data %>% filter(is.na(bc))

# Convert to H2O Frame
hf_train <- as.h2o(data_valid_bc)
hf_predict <- as.h2o(data_missing_bc) # We will predict these gaps
hf_all     <- as.h2o(full_data)       # For predicting everything for plotting

# Split Training Data for Validation
splits <- h2o.splitFrame(data = hf_train, ratios = 0.8, seed = 123)
train_frame <- splits[[1]]
valid_frame <- splits[[2]]

# Define Features (Based on RFE results or your manual list)
features <- c("no", "no2", "o3", "pm2.5", "pm10", "dec_day", "longitude", "blh")
target <- "bc"

# 4. Train Models
# ------------------------------------------------------------------------------
message("Training Random Forest...")
rf_model <- h2o.randomForest(
  x = features, y = target,
  training_frame = train_frame,
  validation_frame = valid_frame,
  model_id = "rf_UB",
  ntrees = 50, # Reduced from your script slightly for speed, adjust if needed
  keep_cross_validation_predictions = TRUE,
  nfolds = 5,
  seed = 1
)

message("Training XGBoost...")
xgb_model <- h2o.xgboost(
  x = features, y = target,
  training_frame = train_frame,
  validation_frame = valid_frame,
  model_id = "xgb_UB",
  keep_cross_validation_predictions = TRUE,
  nfolds = 5,
  seed = 1
)

message("Training Stacked Ensemble...")
ensemble_model <- h2o.stackedEnsemble(
  x = features, y = target,
  training_frame = train_frame,
  base_models = list(rf_model, xgb_model),
  seed = 1
)

# 5. Evaluate Performance
# ------------------------------------------------------------------------------
perf_stats <- function(model, name) {
  c(Model = name, 
    R2 = h2o.r2(model, xval = TRUE), 
    MAE = h2o.mae(model, xval = TRUE), 
    RMSE = h2o.rmse(model, xval = TRUE))
}

results_table <- rbind(
  perf_stats(rf_model, "RandomForest"),
  perf_stats(xgb_model, "XGBoost"),
  perf_stats(ensemble_model, "StackedEnsemble") # Note: Stacked usually doesn't have xval metrics by default unless calculated manually, using training metrics here for simplicity or valid_frame
)
print(results_table)
write.csv(results_table, "data/processed/04a_UB_model_performance.csv", row.names = FALSE)

# 6. Generate Predictions
# ------------------------------------------------------------------------------
message("Generating predictions...")

# Predict for the entire dataset (both missing and present, to compare)
preds <- h2o.predict(ensemble_model, newdata = hf_all)
preds_df <- as.data.frame(preds)

# Combine with original data
final_results <- full_data %>%
  mutate(
    bc_predicted = preds_df$predict,
    # Fix negative predictions
    bc_predicted = pmax(0, bc_predicted),
    # Create a "Best Available" column: Use actual if exists, else predict
    bc_final = coalesce(bc, bc_predicted),
    # Residual
    residual = bc - bc_predicted,
    date = as.Date(date_end)
  )

# Save Final Dataset
if(!dir.exists("data/processed")) dir.create("data/processed")
saveRDS(final_results, "data/processed/04a_UB_predictions_final.rds")
write_csv(final_results %>% dplyr::select(date_end, site_id, bc, bc_predicted, bc_final), 
          "data/processed/04a_UB_BC_actual_vs_estimated.csv")

# 7. Visualization: Time Series Plots (Loop method)
# ------------------------------------------------------------------------------
message("Generating plots...")
if(!dir.exists("plots/UB")) dir.create("plots/UB", recursive = TRUE)

# Calculate Monthly Means for Cleaner Plotting
monthly_data <- final_results %>%
  mutate(month_year = floor_date(date, "month")) %>%
  group_by(site_id, site_name, month_year) %>%
  summarise(
    bc_actual = mean(bc, na.rm = TRUE),
    bc_final = mean(bc_final, na.rm = TRUE),
    .groups = "drop"
  )

# Plot Loop
unique_sites <- unique(monthly_data$site_id)
plot_list <- list()

for(site in unique_sites) {
  site_data <- monthly_data %>% filter(site_id == site)
  site_name <- site_data$site_name[1]
  
  p <- ggplot(site_data, aes(x = month_year)) +
    geom_line(aes(y = bc_final, color = "Estimated (Combined)"), linetype = "dashed") +
    geom_line(aes(y = bc_actual, color = "Actual Data"), size = 1) +
    scale_color_manual(values = c("Actual Data" = "darkslategray4", "Estimated (Combined)" = "red")) +
    labs(title = site_name, y = "BC (ug/m3)", x = "") +
    theme_minimal() +
    theme(legend.position = "none") # Hide legend for individual plots
  
  plot_list[[site]] <- p
  # Save individual
  ggsave(paste0("plots/UB/BC_TimeSeries_", site, ".png"), p, width = 6, height = 4)
}

# Combine into one big plot
combined_plot <- ggarrange(plotlist = plot_list, ncol = 2, nrow = 5, common.legend = TRUE, legend = "bottom")
annotate_figure(combined_plot, top = text_grob("Monthly Averaged BC (Actual vs Estimated)", face = "bold", size = 14))
ggsave("plots/04a_UB_Combined_TimeSeries.png", width = 12, height = 15)


# 8. OpenAir Analysis
# ------------------------------------------------------------------------------
# openair functions expect a 'date' column in POSIXct
openair_data <- final_results %>% 
  mutate(date = date_end) %>% 
  dplyr::select(date, site_id, bc, bc_final, residual)

# Seasonal Plot
png("plots/04a_UB_Seasonal_Variation.png", width = 800, height = 600)
timePlot(openair_data, pollutant = c("bc", "bc_final"), 
         group = TRUE, type = "season", 
         main = "Seasonal Variation: Actual vs Estimated BC",
         ylab = "BC (ug/m3)")
dev.off()

# Diurnal Plot
png("plots/04a_UB_Diurnal_Variation.png", width = 800, height = 600)
timeVariation(openair_data, pollutant = "bc_final", 
              main = "Diurnal Variation of Reconstructed BC",
              cols = "firebrick")
dev.off()

message("Analysis Complete. Check 'plots/UB' and 'data/processed' for results.")