# ==============================================================================
# Script: 02_Process_Weather_Data.R
# Purpose: Extract weather variables from NetCDF (.nc) files for specific sites,
#          across years 2009-2020, and combine into a single dataset.
# Note: Requires .nc files (~600MB each) to be present in 'data/weather_raw/'
# ==============================================================================

# 1. Load Packages
# ------------------------------------------------------------------------------
library(tidyverse)  # Data manipulation
library(raster)     # Handling .nc files
library(ncdf4)      # NetCDF interface
library(saqgetr)    # Site metadata
library(lubridate)  # Date parsing

# 2. Define Sites
# ------------------------------------------------------------------------------
# List of 20 target sites
target_sites <- c(
  "gb0048r","gb1055r","gb0886a","gb0031r","gb0033r",
  "gb0036r","gb0038r","gb0043r","gb0617a","gb0754a",
  "gb1028a","gb0620a","gb0567a","gb0851a","gb0580a",
  "gb0839a","gb0613a","gb0995a","gb0658a","gb0864a"
)

# Fetch and filter site metadata
sites_df <- get_saq_sites() %>% 
  filter(site %in% target_sites) %>%
  dplyr::select(site_id = site, site_name, latitude, longitude) %>%
  arrange(site_name)

# 3. Setup Processing Parameters
# ------------------------------------------------------------------------------
years <- 2009:2020
# List of variables to extract from the NetCDF files
weather_vars <- c("t2m", "d2m", "blh", "slhf", "sshf", "sp", "tp", "ptype", "u10", "v10", "i10fg", "tcc", "e")

# 4. Main Processing Loop
# ------------------------------------------------------------------------------
all_years_weather <- list() # List to store results for each year

for (year in years) {
  
  # Define file path for this year
  nc_file <- paste0("data/weather_raw/weather_", year, ".nc")
  
  if (file.exists(nc_file)) {
    message(paste("Processing weather data for year:", year, "..."))
    
    # Placeholder for this year's combined data
    year_data_list <- list()
    
    # Loop through each weather variable (t2m, d2m, etc.)
    for (var in weather_vars) {
      
      # Load the specific variable layer
      # suppressWarnings to hide CRS warnings common with NetCDF
      r_brick <- suppressWarnings(brick(nc_file, varname = var))
      
      # Extract values at site coordinates
      extracted_values <- raster::extract(r_brick, cbind(sites_df$longitude, sites_df$latitude))
      
      # Convert to Data Frame
      df_extract <- as.data.frame(extracted_values)
      
      # Clean column names to get timestamps
      # Raster extraction usually names columns like "X2009.01.01.00.00.00"
      # We remove the "X" and parse
      col_dates <- sub('^.', '', names(df_extract))
      
      # Add site info and reshape to long format
      df_extract <- cbind(sites_df[, c("site_id", "site_name")], df_extract)
      
      df_long <- df_extract %>%
        pivot_longer(
          cols = -c(site_id, site_name),
          names_to = "raw_date",
          values_to = var # Name the value column after the variable (e.g., "t2m")
        )
      
      year_data_list[[var]] <- df_long
    }
    
    # Merge all variables for this year into one dataframe
    # We use Reduce + merge to join them all by site and raw_date
    year_combined <- Reduce(function(x, y) merge(x, y, by = c("site_id", "site_name", "raw_date")), year_data_list)
    
    # Clean up the date column once
    year_combined <- year_combined %>%
      mutate(date_end = as_datetime(raw_date, format = "%Y.%m.%d.%H.%M.%S")) %>%
      dplyr::select(-raw_date) %>%
      dplyr::select(site_id, site_name, date_end, everything())
    
    all_years_weather[[as.character(year)]] <- year_combined
    
  } else {
    warning(paste("File not found:", nc_file, "- Skipping year", year))
  }
}

# 5. Combine All Years and Save
# ------------------------------------------------------------------------------
if (length(all_years_weather) > 0) {
  final_weather_df <- bind_rows(all_years_weather)
  
  # Save cleaned file
  if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)
  saveRDS(final_weather_df, "data/processed/02_weather_2009_2020_clean.rds")
  
  message("Success! Weather data processed and saved to data/processed/02_weather_2009_2020_clean.rds")
  
} else {
  warning("No weather data was processed. Please check if .nc files are in data/weather_raw/")
}