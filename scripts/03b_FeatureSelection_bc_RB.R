# ==============================================================================
# Script: 03b_FeatureSelection_bc_RB.R
# Purpose: Merge Rural Background (RB) data with weather data, perform feature 
#          engineering, and run Recursive Feature Elimination (RFE) to find
#          important predictors for Black Carbon (BC).
# ==============================================================================

# 1. Load Packages
# ------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(caret)
library(randomForest)
library(data.table)

# 2. Load Processed Data
# ------------------------------------------------------------------------------
# Load the cleaned RB pollution data (from step 01b)
if (file.exists("data/processed/01b_RB_data_clean.rds")) {
  data_rb <- readRDS("data/processed/01b_RB_data_clean.rds")
} else {
  stop("RB Data file not found. Please run 01b_Download_Data_DataSites_RB.R first.")
}

# Load the cleaned Weather data (from step 02)
if (file.exists("data/processed/02_weather_2009_2020_clean.rds")) {
  weather_data <- readRDS("data/processed/02_weather_2009_2020_clean.rds")
} else {
  stop("Weather file not found. Please run 02_Process_Weather_Data.R first.")
}

# 3. Merge Pollution and Weather
# ------------------------------------------------------------------------------
message("Merging pollution and weather data...")

# Join by site_id, site_name, and date_end
full_data <- data_rb %>%
  inner_join(weather_data, by = c("site_id", "site_name", "date_end"))

# 4. Feature Engineering
# ------------------------------------------------------------------------------
full_data <- full_data %>%
  mutate(
    # Decimal Day
    dec_day = decimal_date(date_end),
    
    # Wind Speed (Magnitude)
    ws = sqrt(u10^2 + v10^2),
    
    # Wind Direction (Degrees 0-360)
    wd_rad = atan2(u10/ws, v10/ws),
    wd = (wd_rad * 180 / pi) + 180
  ) %>%
  dplyr::select(-wd_rad)

# 5. Patch in "Extra" Pollutant Data
# ------------------------------------------------------------------------------
extra_path <- "data/RB_air_pollutants_extra.csv"

if (file.exists(extra_path)) {
  message("Updating with extra pollutant data...")
  extra_df <- read_csv(extra_path, show_col_types = FALSE)
  
  # Ensure standard names and formats
  if ("date_end" %in% names(extra_df)) extra_df$date_end <- as_datetime(extra_df$date_end)
  if ("site" %in% names(extra_df)) extra_df <- rename(extra_df, site_id = site)
  
  # Filter for useful rows (where pm2.5 is present)
  extra_clean <- extra_df %>% filter(!is.na(pm2.5))
  
  # Update existing rows safely without creating duplicates
  if (nrow(extra_clean) > 0) {
    full_data <- full_data %>%
      rows_update(extra_clean, by = c("date_end", "site_id"), unmatched = "ignore")
  }
}

# 6. Prepare Data for Machine Learning
# ------------------------------------------------------------------------------
# Define predictors (excluding CO as per your original script logic)
predictors <- c(
  # Temporal
  "dec_day", "Month", "Weekday", "Hour",
  # Weather
  "t2m", "d2m", "blh", "slhf", "sshf", "sp", "tp", "u10", "v10", "ws", "wd", "tcc", "e",
  # Other Pollutants
  "nox", "no2", "no", "pm10", "pm2.5", "o3", "so2" 
  # Note: 'so2' was kept in your RB script but removed in UB. 
  # If it causes too many missing rows, remove it from this list.
)

target <- "bc"

# Select only relevant columns and remove incomplete rows
model_data <- full_data %>%
  dplyr::select(all_of(c(target, predictors))) %>%
  filter(complete.cases(.)) # Remove rows with NAs

# 7. Run Recursive Feature Elimination (RFE)
# ------------------------------------------------------------------------------
message("Starting Feature Selection (RFE)...")

set.seed(123)
# Sample 3000 rows (same as UB analysis)
if(nrow(model_data) > 3000) {
  sample_indices <- sample(nrow(model_data), 3000)
  data_sample <- model_data[sample_indices, ]
} else {
  data_sample <- model_data
}

# Define RFE control
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

# Run RFE
set.seed(123)
rfe_results <- rfe(
  x = data_sample[, predictors], 
  y = data_sample[[target]],
  sizes = c(1:15), 
  rfeControl = ctrl
)

# 8. Output and Save Results
# ------------------------------------------------------------------------------
print(rfe_results)

selected_vars <- predictors(rfe_results)
message("Top selected variables: ")
print(selected_vars)

# Plot
p <- ggplot(rfe_results, metric = "Rsquared") +
  theme_bw() +
  labs(title = "RFE Feature Selection Results (RB Sites)")

print(p)

# Save Plot
if (!dir.exists("plots")) dir.create("plots")
ggsave("plots/03b_RFE_results_RB.png", plot = p, width = 6, height = 4)

# Save Result Object
saveRDS(rfe_results, "data/processed/03b_rfe_results_RB.rds")