# ==============================================================================
# Script: 05_ploting_timeSeries_UBRB.R
# Purpose: Calculate BC/PM ratios, aggregate data to yearly timescales, and 
#          generate final time-series and ratio plots for the thesis.
# ==============================================================================

# 1. Load Packages
# ------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(openair)
library(zoo)
library(scales)

# 2. Load and Combine Data
# ------------------------------------------------------------------------------
message("Loading UB and RB prediction data...")

# Load UB (Urban Background)
if(file.exists("data/processed/04a_UB_predictions_final.rds")){
  df_ub <- readRDS("data/processed/04a_UB_predictions_final.rds") %>%
    mutate(site_type = "Urban Background")
} else { stop("UB data missing. Run 04a first.") }

# Load RB (Rural Background)
if(file.exists("data/processed/04b_RB_predictions_final.rds")){
  df_rb <- readRDS("data/processed/04b_RB_predictions_final.rds") %>%
    mutate(site_type = "Rural Background")
} else { stop("RB data missing. Run 04b first.") }

# Combine into one master dataframe
all_data <- bind_rows(df_ub, df_rb) %>%
  mutate(date = as.Date(date_end))

# 3. Calculate Ratios & Pre-process
# ------------------------------------------------------------------------------
# Calculate ratios for both Original (bc) and Gap-Filled (bc_final)
all_data <- all_data %>%
  mutate(
    # Handle division by zero/NA safely
    bc_pm10_ratio_orig = ifelse(pm10 > 0, bc / pm10, NA),
    bc_pm10_ratio_fill = ifelse(pm10 > 0, bc_final / pm10, NA),
    
    bc_pm25_ratio_orig = ifelse(pm2.5 > 0, bc / pm2.5, NA),
    bc_pm25_ratio_fill = ifelse(pm2.5 > 0, bc_final / pm2.5, NA)
  )

# Define Regions for Plotting (Mapping sites to regions)
all_data <- all_data %>%
  mutate(
    Region = case_when(
      site_name %in% c("Belfast Centre") ~ "Northern Ireland",
      site_name %in% c("Cardiff Centre", "Narberth") ~ "Wales",
      site_name %in% c("Edinburgh St Leonards", "Glasgow Townhead", "Auchencorth Moss", "Bush Estate") ~ "Scotland",
      TRUE ~ "England" # Default to England for others
    )
  )

# 4. Aggregation (Daily -> Monthly -> Yearly)
# ------------------------------------------------------------------------------
message("Aggregating data...")

# Aggregation Function to keep code DRY
aggregate_temporal <- function(df, time_col) {
  df %>%
    group_by(site_id, site_name, site_type, Region, time_group = floor_date(date, time_col)) %>%
    summarise(
      bc_pm25_ratio_orig = mean(bc_pm25_ratio_orig, na.rm = TRUE),
      bc_pm25_ratio_fill = mean(bc_pm25_ratio_fill, na.rm = TRUE),
      bc_pm10_ratio_orig = mean(bc_pm10_ratio_orig, na.rm = TRUE),
      bc_pm10_ratio_fill = mean(bc_pm10_ratio_fill, na.rm = TRUE),
      bc_orig = mean(bc, na.rm = TRUE),
      bc_final = mean(bc_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(date = time_group)
}

# Create aggregated datasets
monthly_data <- aggregate_temporal(all_data, "month")
yearly_data  <- aggregate_temporal(all_data, "year")

# 5. Plotting: Yearly Ratios (Step Plots)
# ------------------------------------------------------------------------------
if(!dir.exists("plots/Final_Analysis")) dir.create("plots/Final_Analysis", recursive = TRUE)

message("Generating Yearly Ratio Plots...")

# Function to draw the specific "Step" plots from your thesis
plot_yearly_ratios <- function(data_subset, title_text, file_name, y_limit = 0.3) {
  
  p <- ggplot(data_subset, aes(x = date)) +
    # Step line for Original Data
    geom_step(aes(y = bc_pm25_ratio_orig, color = "Original BC/PM2.5"), size = 1, direction = "hv") +
    # Step line for Gap-Filled Data
    geom_step(aes(y = bc_pm25_ratio_fill, color = "Gap-Filled BC/PM2.5"), size = 1, linetype = "dashed", direction = "hv") +
    
    facet_wrap(~site_name, ncol = 3) +
    
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_y_continuous(limits = c(0, y_limit)) +
    scale_color_manual("", values = c("Original BC/PM2.5" = "#004488", "Gap-Filled BC/PM2.5" = "gold3")) +
    
    labs(title = title_text, x = "", y = "BC / PM2.5 Yearly Ratio") +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  ggsave(file_name, p, width = 12, height = 6, dpi = 300)
}

# --- Plot 1: England UB Sites ---
ub_england <- yearly_data %>% filter(site_type == "Urban Background", Region == "England")
plot_yearly_ratios(ub_england, "BC/PM2.5 Yearly Ratios (UB - England)", "plots/Final_Analysis/01_UB_England_Ratios.png")

# --- Plot 2: Other Regions UB Sites ---
ub_others <- yearly_data %>% filter(site_type == "Urban Background", Region != "England")
plot_yearly_ratios(ub_others, "BC/PM2.5 Yearly Ratios (UB - Scotland/Wales/NI)", "plots/Final_Analysis/02_UB_Regions_Ratios.png")

# --- Plot 3: Rural Background Sites ---
rb_sites <- yearly_data %>% filter(site_type == "Rural Background")
plot_yearly_ratios(rb_sites, "BC/PM2.5 Yearly Ratios (Rural Background)", "plots/Final_Analysis/03_RB_All_Ratios.png", y_limit = 0.15)


# 6. NAEI Comparison (Emission Inventory)
# ------------------------------------------------------------------------------
naei_path <- "data/naei_BC_PM25_ratio_2009to2020.csv"

if(file.exists(naei_path)) {
  message("Plotting NAEI Comparison...")
  naei_data <- read_csv(naei_path, show_col_types = FALSE)
  
  p_naei <- ggplot(naei_data, aes(x = Date, y = bc_pm25_ratio)) +
    geom_step(size = 1.2, direction = "hv") +
    scale_x_continuous(breaks = 2009:2020) +
    labs(
      title = expression(paste("NAEI Inventory: BC / ", PM[2.5], " Yearly Ratio (2009-2020)")),
      y = "Ratio", x = "Year"
    ) +
    theme_bw() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  
  ggsave("plots/Final_Analysis/04_NAEI_Ratio.png", p_naei, width = 8, height = 6)
} else {
  warning("NAEI data file not found (data/naei_BC_PM25_ratio_2009to2020.csv). Skipping plot.")
}

# 7. OpenAir Time Variation Plots
# ------------------------------------------------------------------------------
message("Generating OpenAir Time Variation plots...")

# OpenAir requires a specific 'date' column in POSIXct
# We will do this for one representative site (e.g., London N. Kensington) 
# or aggregate all sites. Let's do London N. Kensington as per your original script.

target_site <- "London N. Kensington"
nk_data <- all_data %>% 
  filter(site_name == target_site) %>%
  dplyr::select(date = date_end, bc, bc_final) %>%
  rename(Original = bc, GapFilled = bc_final)

if(nrow(nk_data) > 0) {
  png("plots/Final_Analysis/05_TimeVariation_LondonNK.png", width = 800, height = 500)
  timeVariation(nk_data, pollutant = c("Original", "GapFilled"),
                main = paste("BC Variation:", target_site),
                col = c("firebrick", "dodgerblue"))
  dev.off()
}

message("Done! All analysis and plotting complete.")