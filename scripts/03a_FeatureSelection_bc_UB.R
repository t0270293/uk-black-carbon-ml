# ==============================================================================
# Script: 03a_FeatureSelection_bc_UB.R
# Purpose: Merge Urban Background (UB) data with weather data, perform feature 
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
# Load the cleaned UB pollution data (from step 01a)
if (file.exists("data/processed/01a_UB_data_clean.rds")) {
  data_ub <- readRDS("data/processed/01a_UB_data_clean.rds")
} else {
  stop("UB Data file not found. Please run 01a_Download_Data_DataSites_UB.R first.")
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
# Note: We use inner_join to keep only rows where we have BOTH pollution and weather
full_data <- data_ub %>%
  inner_join(weather_data, by = c("site_id", "site_name", "date_end"))

# 4. Feature Engineering
# ------------------------------------------------------------------------------
full_data <- full_data %>%
  mutate(
    # Decimal Day (useful for trend analysis)
    dec_day = decimal_date(date_end),
    
    # Wind Speed (Magnitude of u10 and v10 vectors)
    ws = sqrt(u10^2 + v10^2),
    
    # Wind Direction (Degrees 0-360)
    # atan2(y, x) returns radians. We convert to degrees and adjust orientation.
    wd_rad = atan2(u10/ws, v10/ws),
    wd = (wd_rad * 180 / pi) + 180
  ) %>%
  dplyr::select(-wd_rad) # remove temporary radian column

# 5. Patch in "Extra" Pollutant Data
# ------------------------------------------------------------------------------
extra_path <- "data/UB_air_pollutants_extra.csv"

if (file.exists(extra_path)) {
  message("Updating with extra pollutant data...")
  extra_df <- read_csv(extra_path, show_col_types = FALSE)
  
  # Ensure standard names for joining
  if ("date_end" %in% names(extra_df)) extra_df$date_end <- as_datetime(extra_df$date_end)
  if ("site" %in% names(extra_df)) extra_df <- rename(extra_df, site_id = site)
  
  # Update rows where pm2.5 is not NA in the extra file
  # We use rows_update to safely overwrite existing values
  extra_clean <- extra_df %>% filter(!is.na(pm2.5))
  
  # Only update if the columns actually exist in both
  if (nrow(extra_clean) > 0) {
    full_data <- full_data %>%
      rows_update(extra_clean, by = c("date_end", "site_id"), unmatched = "ignore")
  }
}

# 6. Prepare Data for Machine Learning
# ------------------------------------------------------------------------------
# Define the predictors we want to test
# (Explicitly listing them is safer than using column numbers)
predictors <- c(
  # Temporal
  "dec_day", "Month", "Weekday", "Hour",
  # Weather
  "t2m", "d2m", "blh", "slhf", "sshf", "sp", "tp", "u10", "v10", "ws", "wd", "tcc", "e",
  # Other Pollutants (potential proxies)
  "nox", "no2", "no", "pm10", "pm2.5", "o3"
)

target <- "bc"

# Select only relevant columns and remove incomplete rows
model_data <- full_data %>%
  dplyr::select(all_of(c(target, predictors))) %>%
  filter(complete.cases(.)) # Remove rows with any NA values

# 7. Run Recursive Feature Elimination (RFE)
# ------------------------------------------------------------------------------
message("Starting Feature Selection (RFE)...")

# RFE is computationally expensive. We sample 3000 rows as per your original script.
set.seed(123)
sample_indices <- sample(nrow(model_data), 3000)
data_sample <- model_data[sample_indices, ]

# Define RFE control (Random Forest functions, 10-fold CV)
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

# Run RFE
# x = predictors, y = target
set.seed(123)
rfe_results <- rfe(
  x = data_sample[, predictors], 
  y = data_sample[[target]],
  sizes = c(1:15), # Check subsets of size 1 to 15
  rfeControl = ctrl
)

# 8. Output and Save Results
# ------------------------------------------------------------------------------
print(rfe_results)

# List selected variables
selected_vars <- predictors(rfe_results)
message("Top selected variables: ")
print(selected_vars)

# Plot
p <- ggplot(rfe_results, metric = "Rsquared") +
  theme_bw() +
  labs(title = "RFE Feature Selection Results (UB Sites)")

print(p)

# Save the plot
if (!dir.exists("plots")) dir.create("plots")
ggsave("plots/03a_RFE_results_UB.png", plot = p, width = 6, height = 4)

# Save the RFE object in case you want to inspect it later
saveRDS(rfe_results, "data/processed/03a_rfe_results_UB.rds")