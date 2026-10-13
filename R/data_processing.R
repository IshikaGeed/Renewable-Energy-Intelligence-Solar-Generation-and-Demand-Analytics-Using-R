# ==============================================================================
# Renewable Energy Intelligence - Data Processing Pipeline
# Project: Renewable Energy Intelligence Using R
# Description: Ingests, validates, cleans, and engineers features for the
#              Grid-India PMU 1-hour interval dataset (Demand, Solar, Wind).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

#' Ingest and clean raw electricity dataset
#'
#' @param input_path Path to raw CSV file
#' @param output_csv Path to save cleaned CSV
#' @param output_rds Path to save fast-loading RDS
#' @return A cleaned tibble with engineered features
clean_raw_data <- function(input_path = "data/raw/india_electricity_re_2024_2025_raw.csv",
                           output_csv = "data/processed/india_electricity_re_clean.csv",
                           output_rds = "data/processed/india_electricity_re_clean.rds") {
  
  if (!file.exists(input_path)) {
    # Check if relative to app root or current working dir
    alt_path <- file.path("..", input_path)
    if (file.exists(alt_path)) {
      input_path <- alt_path
    } else {
      stop(sprintf("Raw dataset not found at '%s'. Please ensure the raw CSV is present.", input_path))
    }
  }
  
  # Read raw CSV
  raw_df <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)
  
  # Standardize column names
  col_map <- c(
    "Timestamp" = "Timestamp",
    "Demand_MW" = "Demand_MW",
    "Wind_MW" = "Wind_MW",
    "Solar_MW" = "Solar_MW",
    "Total_Generation_MW" = "Total_Generation_MW"
  )
  
  # Handle alternate column headers if present
  colnames(raw_df) <- gsub(" ", "_", colnames(raw_df))
  colnames(raw_df) <- gsub("[()]", "", colnames(raw_df))
  
  # Find matching columns
  ts_col <- grep("Timestamp|Time|Date", colnames(raw_df), value = TRUE, ignore.case = TRUE)[1]
  dem_col <- grep("Demand", colnames(raw_df), value = TRUE, ignore.case = TRUE)[1]
  wind_col <- grep("Wind", colnames(raw_df), value = TRUE, ignore.case = TRUE)[1]
  solar_col <- grep("Solar", colnames(raw_df), value = TRUE, ignore.case = TRUE)[1]
  tot_col <- grep("Total_Generation|TotalGen", colnames(raw_df), value = TRUE, ignore.case = TRUE)[1]
  
  if (is.na(ts_col) || is.na(dem_col) || is.na(wind_col) || is.na(solar_col)) {
    stop("Input data is missing one or more required columns (Timestamp, Demand, Wind, Solar).")
  }
  
  # Parse Timestamps
  # Support formats: "YYYY-MM-DD HH:MM:SS" or "DD-MM-YYYY HH:MM:SS"
  ts_raw <- trimws(as.character(raw_df[[ts_col]]))
  parsed_ts <- suppressWarnings(as.POSIXct(ts_raw, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Kolkata"))
  if (all(is.na(parsed_ts))) {
    parsed_ts <- suppressWarnings(as.POSIXct(ts_raw, format = "%d-%m-%Y %H:%M:%S", tz = "Asia/Kolkata"))
  }
  
  # Filter out unparseable rows (such as appended summary rows like 'Average', 'Sum', 'Minimum')
  valid_mask <- !is.na(parsed_ts)
  
  clean_df <- tibble(
    Timestamp = parsed_ts[valid_mask],
    Demand_MW = suppressWarnings(as.numeric(raw_df[[dem_col]][valid_mask])),
    Wind_MW   = suppressWarnings(as.numeric(raw_df[[wind_col]][valid_mask])),
    Solar_MW  = suppressWarnings(as.numeric(raw_df[[solar_col]][valid_mask]))
  )
  
  if (!is.na(tot_col)) {
    clean_df$Total_Grid_Gen_MW <- suppressWarnings(as.numeric(raw_df[[tot_col]][valid_mask]))
  } else {
    clean_df$Total_Grid_Gen_MW <- clean_df$Demand_MW
  }
  
  # Remove rows where Demand or Generation are NA or completely invalid
  clean_df <- clean_df %>%
    filter(!is.na(Demand_MW), Demand_MW > 0)
  
  # Domain Data Cleaning & Telemetry Sanitization:
  # In solar SCADA systems, night-time values often show small negative numbers (-0.1 to -69 MW)
  # due to inverter standby power consumption or sensor zero-drift calibration.
  # We clamp Solar_MW and Wind_MW to 0 as physical energy generation cannot be negative.
  clean_df <- clean_df %>%
    mutate(
      Solar_MW = pmax(0, coalesce(Solar_MW, 0)),
      Wind_MW  = pmax(0, coalesce(Wind_MW, 0))
    )
  
  # Sort chronologically and deduplicate
  clean_df <- clean_df %>%
    distinct(Timestamp, .keep_all = TRUE) %>%
    arrange(Timestamp)
  
  # Feature Engineering for Business Intelligence
  clean_df <- clean_df %>%
    mutate(
      Date       = as.Date(Timestamp, tz = "Asia/Kolkata"),
      Year       = lubridate::year(Timestamp),
      Month_Num  = lubridate::month(Timestamp),
      Month      = factor(lubridate::month(Timestamp, label = TRUE, abbr = TRUE),
                          levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
      Hour       = lubridate::hour(Timestamp),
      DayOfWeek  = factor(lubridate::wday(Timestamp, label = TRUE, abbr = TRUE, week_start = 1),
                          levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
      IsWeekend  = DayOfWeek %in% c("Sat", "Sun"),
      
      # India Meteorological Department (IMD) Standard Seasons
      Season = factor(case_when(
        Month_Num %in% c(12, 1, 2) ~ "Winter",
        Month_Num %in% c(3, 4, 5)   ~ "Summer / Pre-Monsoon",
        Month_Num %in% c(6, 7, 8, 9)~ "Monsoon",
        Month_Num %in% c(10, 11)    ~ "Post-Monsoon",
        TRUE                        ~ "Other"
      ), levels = c("Winter", "Summer / Pre-Monsoon", "Monsoon", "Post-Monsoon")),
      
      # Aggregated Renewable Generation
      Renewable_MW = Solar_MW + Wind_MW,
      
      # Renewable Penetration Rates
      Renewable_Share_Pct = round((Renewable_MW / Demand_MW) * 100, 2),
      Solar_Share_Pct     = round((Solar_MW / Demand_MW) * 100, 2),
      Wind_Share_Pct      = round((Wind_MW / Demand_MW) * 100, 2),
      
      # Net Load (Duck Curve metric: Conventional generation required)
      Residual_Demand_MW  = Demand_MW - Renewable_MW,
      
      # Shortfall / Low Renewable Contribution Tier
      RE_Tier = factor(case_when(
        Renewable_Share_Pct < 5  ~ "Critical Low (<5%)",
        Renewable_Share_Pct < 15 ~ "Low (5-15%)",
        Renewable_Share_Pct < 25 ~ "Moderate (15-25%)",
        TRUE                     ~ "High (>25%)"
      ), levels = c("Critical Low (<5%)", "Low (5-15%)", "Moderate (15-25%)", "High (>25%)"))
    ) %>%
    # Calculate hourly Net Load Ramp Rate (MW/hour)
    mutate(
      Net_Ramp_MW = c(0, diff(Residual_Demand_MW))
    )
  
  # Ensure target directory exists
  dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(output_rds), recursive = TRUE, showWarnings = FALSE)
  
  # Export clean data
  write.csv(clean_df, output_csv, row.names = FALSE)
  saveRDS(clean_df, output_rds)
  
  message(sprintf("Successfully processed %d records from %s to %s.", 
                  nrow(clean_df), min(clean_df$Date), max(clean_df$Date)))
  
  return(clean_df)
}

#' Load clean dataset with automatic fallback and caching
#'
#' @param cache_rds Path to clean RDS file
#' @param raw_csv Path to raw CSV file
#' @return Preprocessed tibble
load_clean_data <- function(cache_rds = "data/processed/india_electricity_re_clean.rds",
                            raw_csv   = "data/raw/india_electricity_re_2024_2025_raw.csv") {
  
  # Check if RDS cache exists
  if (file.exists(cache_rds)) {
    df <- readRDS(cache_rds)
    return(df)
  }
  
  # Check relative to app if launched from subfolder
  alt_rds <- file.path("..", cache_rds)
  if (file.exists(alt_rds)) {
    return(readRDS(alt_rds))
  }
  
  # If RDS doesn't exist, build from raw CSV
  message("RDS cache not found. Rebuilding clean dataset from raw data...")
  df <- clean_raw_data(input_path = raw_csv, output_rds = cache_rds)
  return(df)
}

#' Generate summary diagnostics for data exploration
#'
#' @param df Cleaned data frame
#' @return Named list with key dataset diagnostics
get_data_diagnostics <- function(df) {
  list(
    total_records    = nrow(df),
    start_date       = min(df$Timestamp),
    end_date         = max(df$Timestamp),
    total_days       = as.numeric(difftime(max(df$Date), min(df$Date), units = "days")) + 1,
    avg_demand_gw    = mean(df$Demand_MW, na.rm = TRUE) / 1000,
    peak_demand_gw   = max(df$Demand_MW, na.rm = TRUE) / 1000,
    peak_solar_gw    = max(df$Solar_MW, na.rm = TRUE) / 1000,
    peak_wind_gw     = max(df$Wind_MW, na.rm = TRUE) / 1000,
    max_re_share_pct = max(df$Renewable_Share_Pct, na.rm = TRUE),
    avg_re_share_pct = mean(df$Renewable_Share_Pct, na.rm = TRUE),
    total_demand_twh = sum(df$Demand_MW, na.rm = TRUE) / 1e6, # MWh to TWh
    total_solar_twh  = sum(df$Solar_MW, na.rm = TRUE) / 1e6,
    total_wind_twh   = sum(df$Wind_MW, na.rm = TRUE) / 1e6,
    total_re_twh     = sum(df$Renewable_MW, na.rm = TRUE) / 1e6
  )
}
