# ==============================================================================
# Script: 00_Download_NAEI_grid_data.R
# Purpose: Download Black Carbon (BC) and PM2.5 point source data from NAEI,
#          match them to specific monitoring sites spatially, and calculate ratios.
# Output: A cleaned dataset of emissions near specific sites.
# ==============================================================================

# 1. Load necessary packages
# ------------------------------------------------------------------------------
library(tidyverse)  # Includes dplyr, readr, tidyr, ggplot2
library(saqgetr)    # For accessing air quality site metadata
library(sf)         # For spatial operations

# Create a directory for raw data to keep the project clean
if (!dir.exists("data/NAEI_raw")) dir.create("data/NAEI_raw", recursive = TRUE)

# 2. Define Monitoring Sites
# ------------------------------------------------------------------------------
# Fetch all sites and filter for GB
saq_sites <- get_saq_sites(file = NA) %>% 
  dplyr::filter(country_iso_code == "GB")

# List of specific site IDs to analyze
target_sites <- c(
  "gb1028a", "gb0620a", "gb0851a", "gb0580a", "gb0839a",
  "gb0995a", "gb0613a", "gb0658a", "gb0567a", "gb0864a",
  "gb0048r", "gb1055r", "gb0886a", "gb0617a", "gb0036r",
  "gb0043r", "gb0754a", "gb0031r", "gb0033r", "gb0038r"
)

# Filter sites and select relevant columns
sites_selected <- saq_sites %>%
  dplyr::filter(site %in% target_sites) %>%
  dplyr::select(site, site_name, latitude, longitude)

# Convert sites to British National Grid (EPSG:27700) for accurate distance calc
sites_sf <- st_as_sf(sites_selected, coords = c("longitude", "latitude"), crs = 4326)
sites_bng <- st_transform(sites_sf, crs = 27700)

# Extract Easting/Northing into columns and drop geometry for non-spatial joins
sites_bng_df <- sites_bng %>%
  mutate(
    Easting_Site = st_coordinates(.)[,1],
    Northing_Site = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()

# 3. Helper Function to Download and Process NAEI Data
# ------------------------------------------------------------------------------
# This function handles the download, unzip, and cleaning for any pollutant
process_naei_pollutant <- function(pollutant_code, year_range) {
  
  all_years_list <- list()
  
  for (year in year_range) {
    message(paste("Processing", pollutant_code, "for year:", year))
    
    # Construct URL and paths
    # Note: pollutant_code in URL is usually lowercase (e.g., "bc" or "pm2_5")
    url <- paste0("https://naei.energysecurity.gov.uk/data/maps/download-gridded-emissions/22/", pollutant_code, "/", year)
    zip_name <- paste0("data/NAEI_raw/", pollutant_code, "_", year, ".zip")
    extract_dir <- paste0("data/NAEI_raw/", pollutant_code, "_", year)
    
    # Download if not already present
    if (!file.exists(zip_name)) {
      tryCatch({
        download.file(url, destfile = zip_name, mode = "wb", quiet = TRUE)
      }, error = function(e) message(paste(" - Failed to download:", year)))
    }
    
    # Unzip
    if (file.exists(zip_name)) {
      unzip(zip_name, exdir = extract_dir)
      
      # Find the correct CSV file (names vary slightly by year)
      # Search for pattern: point_sources_POLLUTANT_YEAR.csv
      csv_files <- list.files(extract_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
      target_csv <- csv_files[grep("point_sources", csv_files, ignore.case = TRUE)][1]
      
      if (!is.na(target_csv)) {
        df <- read_csv(target_csv, show_col_types = FALSE)
        
        # Standardize Columns
        df$Year <- year
        
        # Normalize 'Emission' column name
        emission_cols <- names(df)[grepl("emiss", names(df), ignore.case = TRUE)]
        if (length(emission_cols) > 0) names(df)[names(df) == emission_cols[1]] <- "Emission"
        
        # Normalize 'Site' column name
        site_cols <- names(df)[grepl("^site$", names(df), ignore.case = TRUE)]
        if (length(site_cols) > 0) names(df)[names(df) == site_cols[1]] <- "Site"
        
        # Select only needed columns to save memory
        if (all(c("Year", "Site", "Easting", "Northing", "Emission") %in% names(df))) {
          all_years_list[[as.character(year)]] <- df %>%
            dplyr::select(Year, Site, Easting, Northing, Emission)
        }
      }
    }
  }
  return(bind_rows(all_years_list))
}

# 4. Download and Process Data
# ------------------------------------------------------------------------------
years <- 2009:2020

# --- Process Black Carbon (BC) ---
message("--- Starting BC Download ---")
bc_data_raw <- process_naei_pollutant("bc", years)

# --- Process PM2.5 ---
message("--- Starting PM2.5 Download ---")
pm25_data_raw <- process_naei_pollutant("pm2_5", years)

# 5. Spatial Matching Logic
# ------------------------------------------------------------------------------
# Function to find the nearest point source for each site per year
find_nearest_emissions <- function(emissions_df, sites_df) {
  
  # Initialize empty list to store results
  results_list <- list()
  
  # Loop through each monitoring site
  for (i in 1:nrow(sites_df)) {
    this_site <- sites_df[i, ]
    
    # Filter emissions for relevant years
    site_matches <- emissions_df %>%
      group_by(Year) %>%
      group_map(~ {
        # Calculate distances for this year's data
        # Note: .x is the dataframe for the current group (Year)
        
        # Euclidean distance calc
        dists <- sqrt((.x$Easting - this_site$Easting_Site)^2 + 
                        (.x$Northing - this_site$Northing_Site)^2)
        
        # Find minimum distance
        min_idx <- which.min(dists)
        min_dist <- dists[min_idx]
        
        # Extract best match
        match <- .x[min_idx, ]
        match$site_id <- this_site$site
        match$site_name <- this_site$site_name
        match$Exact <- ifelse(min_dist == 0, "Yes", "No")
        
        return(match)
      }, .keep = TRUE) %>%
      bind_rows()
    
    results_list[[i]] <- site_matches
  }
  return(bind_rows(results_list))
}

message("--- Matching BC Data to Sites ---")
bc_matched <- find_nearest_emissions(bc_data_raw, sites_bng_df)

message("--- Matching PM2.5 Data to Sites ---")
pm25_matched <- find_nearest_emissions(pm25_data_raw, sites_bng_df)

# 6. Combine and Calculate Ratios
# ------------------------------------------------------------------------------
final_dataset <- bc_matched %>%
  dplyr::select(Year, site_id, site_name, bc_emission = Emission) %>%
  left_join(
    pm25_matched %>% dplyr::select(Year, site_id, pm25_emission = Emission),
    by = c("Year", "site_id")
  ) %>%
  mutate(
    bc_pm25_ratio = signif(bc_emission / pm25_emission, 6)
  )

# 7. Save Result
# ------------------------------------------------------------------------------
# Save the clean data for the next script to use
if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)
write_csv(final_dataset, "data/processed/00_NAEI_emissions_matched.csv")

message("Processing complete. File saved to data/processed/00_NAEI_emissions_matched.csv")
