# ==============================================================================
# Script: 01b_Download_Data_DataSites_RB.R
# Purpose: Download Rural Background (RB) air quality data, fill missing time 
#          gaps, and merge with external datasets (including Detling-specific data).
# ==============================================================================

# 1. Load Packages
# ------------------------------------------------------------------------------
library(tidyverse)  # Covers dplyr, ggplot2, tidyr, lubridate, readr
library(saqgetr)    # For AURN data
library(data.table) # For efficient merging of extra data

# 2. Define Sites and Time Range
# ------------------------------------------------------------------------------
rb_sites_list <- c(
  "gb0048r", "gb1055r", "gb0886a", "gb0617a", "gb0036r",
  "gb0043r", "gb0754a", "gb0031r", "gb0033r", "gb0038r"
)

start_date <- "2009-01-01"
end_date   <- "2020-12-31"

# 3. Download Air Pollution Data
# ------------------------------------------------------------------------------
message("Downloading air quality data...")

raw_obs <- get_saq_observations(
  site = rb_sites_list,
  start = start_date,
  end = end_date,
  variable = c("o3", "no2", "nox", "so2", "co", "pm10", "pm2.5", "bc", "bs"),
  verbose = FALSE
) %>%
  saq_clean_observations(summary = "hour", valid_only = TRUE, spread = TRUE) %>%
  rename(site_id = site) # Standardizing variable name

# Correct the timestamp (shifting by 1 hour to match "date_end" convention)
raw_obs <- raw_obs %>%
  mutate(date_end = date + hours(1)) %>%
  dplyr::select(-date)

# 4. Download Site Metadata
# ------------------------------------------------------------------------------
site_meta <- get_saq_sites() %>%
  dplyr::filter(site %in% rb_sites_list) %>%
  dplyr::select(site_id = site, site_name, latitude, longitude, 
                elevation, site_type, site_area) %>%
  mutate(type = paste(site_area, site_type))

# Merge observations with metadata
data_rb <- raw_obs %>%
  left_join(site_meta, by = "site_id")

# 5. Patch in "Extra" Black Carbon Data (4 Sites)
# ------------------------------------------------------------------------------
extra_bc_path <- "data/BC_extra_data_RB_4sites.csv"

if (file.exists(extra_bc_path)) {
  bc_extra <- read_csv(extra_bc_path, show_col_types = FALSE)
  
  # Ensure date format is correct
  if (is.character(bc_extra$date_end)) {
    bc_extra$date_end <- ymd_hms(bc_extra$date_end)
  }
  
  # Use data.table for efficient update
  setDT(data_rb)
  setDT(bc_extra)
  
  if ("site" %in% names(bc_extra)) setnames(bc_extra, "site", "site_id")
  
  # Update 'bc' in main data with values from extra file
  data_rb[bc_extra, bc := i.bc, on = .(site_id, date_end)]
  
  data_rb <- as_tibble(data_rb) # Convert back to tibble
  
} else {
  warning(paste("File not found:", extra_bc_path, "- skipping extra BC merge."))
}

# 6. Patch in "Detling" Specific Data (gb0886a)
# ------------------------------------------------------------------------------
# The original script merged a specific file 'Detling.csv' just for this site.
detling_path <- "data/Detling.csv"

if (file.exists(detling_path)) {
  message("Merging Detling specific data...")
  detling_df <- read_csv(detling_path, show_col_types = FALSE)
  
  # Ensure date format is correct
  if (is.character(detling_df$date_end)) {
    detling_df$date_end <- ymd_hms(detling_df$date_end)
  }
  
  # Add site_id if missing so we can join it correctly
  detling_df$site_id <- "gb0886a"
  
  # Remove columns that might duplicate (keep only new info or the join keys)
  # Assuming we want to UPDATE existing rows for gb0886a
  # We use rows_patch or coalesce logic. Here is a safe join approach:
  
  data_rb <- data_rb %>%
    left_join(detling_df, by = c("site_id", "date_end"), suffix = c("", ".detling"))
  
  # If Detling.csv brought in new pollution columns or updated existing ones,
  # you can coalesce them here. For example, if it updates 'bc':
  if ("bc.detling" %in% names(data_rb)) {
    data_rb <- data_rb %>%
      mutate(bc = coalesce(bc.detling, bc)) %>%
      dplyr::select(-ends_with(".detling"))
  }
  
} else {
  warning("Detling.csv not found. Skipping Detling-specific merge.")
}

# 7. Fill Time Gaps
# ------------------------------------------------------------------------------
message("Filling missing time gaps for all sites...")

full_time_seq <- seq(
  from = as.POSIXct(paste(start_date, "01:00:00"), tz = "Europe/London"),
  to   = as.POSIXct(paste(end_date, "23:00:00"), tz = "Europe/London"),
  by   = "hour"
)

data_rb_filled <- data_rb %>%
  group_by(site_id) %>%
  complete(date_end = full_time_seq) %>%
  ungroup() %>%
  # Fill metadata for the newly created gaps
  group_by(site_id) %>%
  fill(site_name, latitude, longitude, elevation, type, site_area, .direction = "downup") %>%
  ungroup()

# 8. Create Time Variables & Final Cleanup
# ------------------------------------------------------------------------------
data_rb_final <- data_rb_filled %>%
  mutate(
    Date    = as.Date(date_end),
    Time    = format(date_end, "%H:%M:%S"),
    Hour    = hour(date_end),
    Month   = month(date_end),
    Weekday = wday(date_end, label = FALSE),
    
    # Calculate NO
    no = nox - no2,
    
    # Merge Black Carbon (BC) and Black Smoke (BS)
    bc = pmax(bc, bs, na.rm = TRUE)
  ) %>%
  dplyr::select(-bs) %>%
  arrange(site_id, date_end)

# 9. Save Data
# ------------------------------------------------------------------------------
if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)
saveRDS(data_rb_final, "data/processed/01b_RB_data_clean.rds")

message("Done. RB data saved to data/processed/01b_RB_data_clean.rds")