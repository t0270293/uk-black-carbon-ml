# ==============================================================================
# Script: 01a_Download_Data_DataSites_UB.R
# Purpose: Download Urban Background (UB) air quality data, fill missing time 
#          gaps, and merge with external Black Carbon (BC) datasets.
# ==============================================================================

# 1. Load Packages
# ------------------------------------------------------------------------------
library(tidyverse)  # Covers dplyr, ggplot2, tidyr, lubridate, readr
library(saqgetr)    # For AURN data
library(data.table) # For efficient merging of extra data

# 2. Define Sites and Time Range
# ------------------------------------------------------------------------------
ub_sites_list <- c(
  "gb1028a", "gb0620a", "gb0851a", "gb0580a", "gb0839a",
  "gb0995a", "gb0613a", "gb0658a", "gb0567a", "gb0864a"
)

start_date <- "2009-01-01"
end_date   <- "2021-01-01"

# 3. Download Air Pollution Data
# ------------------------------------------------------------------------------
message("Downloading air quality data...")

# Download observations
raw_obs <- get_saq_observations(
  site = ub_sites_list,
  start = start_date,
  end = end_date,
  variable = c("o3", "no2", "nox", "so2", "co", "pm10", "pm2.5", "bs", "bc"),
  verbose = FALSE
) %>%
  saq_clean_observations(summary = "hour", valid_only = TRUE, spread = TRUE) %>%
  rename(site_id = site) # Standardizing variable name

# Correct the timestamp (shifting by 1 hour to match "date_end" convention)
raw_obs <- raw_obs %>%
  mutate(date_end = date + hours(1)) %>%
  dplyr::select(-date) # Remove old date to avoid confusion

# 4. Download Site Metadata
# ------------------------------------------------------------------------------
site_meta <- get_saq_sites() %>%
  dplyr::filter(site %in% ub_sites_list) %>%
  dplyr::select(site_id = site, site_name, latitude, longitude, 
                elevation, site_type, site_area) %>%
  mutate(type = paste(site_area, site_type))

# Merge observations with metadata
data_ub <- raw_obs %>%
  left_join(site_meta, by = "site_id")

# 5. Patch in "Extra" Black Carbon Data
# ------------------------------------------------------------------------------
# NOTE: Ensure this file is inside your project's 'data' folder!
extra_bc_path <- "data/BC_extra_data_UB_6sites.csv"

if (file.exists(extra_bc_path)) {
  bc_extra <- read_csv(extra_bc_path, show_col_types = FALSE)
  
  # Ensure dates are parsed correctly in the extra file
  # (Adjust format string if your CSV is DD/MM/YYYY)
  if (is.character(bc_extra$date_end)) {
    bc_extra$date_end <- as_datetime(bc_extra$date_end)
  }
  
  # Update the main dataset with extra BC values where available
  # Using data.table syntax here as it was in your original script (efficient)
  setDT(data_ub)
  setDT(bc_extra)
  
  # Rename 'site' to 'site_id' in extra data if needed to match
  if ("site" %in% names(bc_extra)) setnames(bc_extra, "site", "site_id")
  
  # Update 'bc' column in data_ub using values from bc_extra
  data_ub[bc_extra, bc := i.bc, on = .(site_id, date_end)]
  
  # Convert back to tibble (standard dataframe)
  data_ub <- as_tibble(data_ub)
  
} else {
  warning(paste("File not found:", extra_bc_path, "- skipping extra BC merge."))
}

# 6. Fill Time Gaps (The "Complete" Step)
# ------------------------------------------------------------------------------
message("Filling missing time gaps for all sites...")

# Create the full sequence of hours expected
full_time_seq <- seq(
  from = as.POSIXct(paste(start_date, "01:00:00"), tz = "Europe/London"),
  to   = as.POSIXct(paste("2020-12-31", "23:00:00"), tz = "Europe/London"),
  by   = "hour"
)

# Use tidyr::complete to ensure every site has every hour represented
# This replaces the 10 repeated blocks in your old code
data_ub_filled <- data_ub %>%
  group_by(site_id) %>%
  complete(date_end = full_time_seq) %>%
  ungroup() %>%
  # Re-fill metadata (site_name, etc.) for the newly created NA rows
  group_by(site_id) %>%
  fill(site_name, latitude, longitude, elevation, type, site_area, .direction = "downup") %>%
  ungroup()

# 7. Create Time Variables
# ------------------------------------------------------------------------------
data_ub_final <- data_ub_filled %>%
  mutate(
    # Time components
    Date    = as.Date(date_end),
    Time    = format(date_end, "%H:%M:%S"),
    Hour    = hour(date_end),
    Month   = month(date_end),
    Weekday = wday(date_end, label = FALSE), # 1=Sunday, 2=Monday...
    
    # Calculate NO (NO = NOx - NO2)
    no = nox - no2,
    
    # Merge Black Carbon (BC) and Black Smoke (BS)
    # If BC is missing, try to use BS (pmax ignores NAs)
    bc = pmax(bc, bs, na.rm = TRUE)
  ) %>%
  dplyr::select(-bs) %>% # Remove BS column as it is now integrated
  arrange(site_id, date_end)

# 8. Save Data
# ------------------------------------------------------------------------------
if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)
saveRDS(data_ub_final, "data/processed/01a_UB_data_clean.rds")

message("Done. UB data saved to data/processed/01a_UB_data_clean.rds")

