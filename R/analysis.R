# ==============================================================================
# Renewable Energy Intelligence - Analytics & Modeling Engine
# Project: Renewable Energy Intelligence Using R
# Description: Implements KPI calculations, time-series forecasting,
#              diurnal profile modeling, and business insights.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(plotly)
  library(scales)
})

# ------------------------------------------------------------------------------
# 1. KPI & Overview Calculations
# ------------------------------------------------------------------------------

#' Calculate high-level executive KPIs from filtered dataset
#'
#' @param df Cleaned data frame
#' @return Named list of formatted KPI metrics
calc_kpis <- function(df) {
  if (nrow(df) == 0) {
    return(list(
      total_solar_twh     = 0,
      total_wind_twh      = 0,
      total_re_twh        = 0,
      total_demand_twh    = 0,
      re_penetration_pct  = 0,
      solar_penetration_pct = 0,
      wind_penetration_pct  = 0,
      avg_demand_gw       = 0,
      peak_demand_gw      = 0,
      peak_solar_gw       = 0,
      peak_wind_gw        = 0,
      peak_re_gw          = 0,
      max_re_share_pct    = 0,
      avg_net_ramp_gw     = 0,
      max_net_ramp_gw     = 0,
      critical_re_hrs     = 0,
      critical_re_pct     = 0,
      total_records       = 0
    ))
  }
  
  tot_dem_mwh <- sum(df$Demand_MW, na.rm = TRUE)
  tot_sol_mwh <- sum(df$Solar_MW, na.rm = TRUE)
  tot_wnd_mwh <- sum(df$Wind_MW, na.rm = TRUE)
  tot_re_mwh  <- sum(df$Renewable_MW, na.rm = TRUE)
  
  crit_hrs <- sum(df$Renewable_Share_Pct < 5, na.rm = TRUE)
  
  list(
    total_solar_twh       = round(tot_sol_mwh / 1e6, 2),
    total_wind_twh        = round(tot_wnd_mwh / 1e6, 2),
    total_re_twh          = round(tot_re_mwh / 1e6, 2),
    total_demand_twh      = round(tot_dem_mwh / 1e6, 2),
    re_penetration_pct    = round((tot_re_mwh / tot_dem_mwh) * 100, 2),
    solar_penetration_pct = round((tot_sol_mwh / tot_dem_mwh) * 100, 2),
    wind_penetration_pct  = round((tot_wnd_mwh / tot_dem_mwh) * 100, 2),
    avg_demand_gw         = round(mean(df$Demand_MW, na.rm = TRUE) / 1000, 2),
    peak_demand_gw        = round(max(df$Demand_MW, na.rm = TRUE) / 1000, 2),
    peak_solar_gw         = round(max(df$Solar_MW, na.rm = TRUE) / 1000, 2),
    peak_wind_gw          = round(max(df$Wind_MW, na.rm = TRUE) / 1000, 2),
    peak_re_gw            = round(max(df$Renewable_MW, na.rm = TRUE) / 1000, 2),
    max_re_share_pct      = round(max(df$Renewable_Share_Pct, na.rm = TRUE), 2),
    avg_net_ramp_gw       = round(mean(abs(df$Net_Ramp_MW), na.rm = TRUE) / 1000, 2),
    max_net_ramp_gw       = round(max(df$Net_Ramp_MW, na.rm = TRUE) / 1000, 2),
    critical_re_hrs       = crit_hrs,
    critical_re_pct       = round((crit_hrs / nrow(df)) * 100, 1),
    total_records         = nrow(df)
  )
}

# ------------------------------------------------------------------------------
# 2. Visualization Functions
# ------------------------------------------------------------------------------

#' Palette definitions
energy_colors <- list(
  demand    = "#1E293B", # Dark Slate
  solar     = "#F59E0B", # Amber / Solar Yellow
  wind      = "#0284C7", # Sky / Wind Blue
  renewable = "#10B981", # Emerald Green
  net_load  = "#8B5CF6", # Violet / Residual
  danger    = "#EF4444", # Red
  grid      = "#E2E8F0"
)

#' Overview Timeline: Demand vs Solar vs Wind
plot_overview_timeline <- function(df, agg = "daily") {
  if (nrow(df) == 0) return(plotly_empty())
  
  if (agg == "daily") {
    plot_data <- df %>%
      group_by(Date) %>%
      summarise(
        Demand    = mean(Demand_MW, na.rm = TRUE) / 1000,
        Solar     = mean(Solar_MW, na.rm = TRUE) / 1000,
        Wind      = mean(Wind_MW, na.rm = TRUE) / 1000,
        Renewable = mean(Renewable_MW, na.rm = TRUE) / 1000,
        .groups = "drop"
      )
    x_var <- plot_data$Date
    x_title <- "Date"
  } else if (agg == "weekly") {
    plot_data <- df %>%
      mutate(Week = lubridate::floor_date(Date, "week")) %>%
      group_by(Week) %>%
      summarise(
        Demand    = mean(Demand_MW, na.rm = TRUE) / 1000,
        Solar     = mean(Solar_MW, na.rm = TRUE) / 1000,
        Wind      = mean(Wind_MW, na.rm = TRUE) / 1000,
        Renewable = mean(Renewable_MW, na.rm = TRUE) / 1000,
        .groups = "drop"
      )
    x_var <- plot_data$Week
    x_title <- "Week Starting"
  } else {
    # Hourly (limit to 14 days if too large for responsiveness)
    if (nrow(df) > 24 * 31) {
      plot_data <- df %>%
        slice_tail(n = 24 * 14) %>%
        transmute(
          Timestamp,
          Demand    = Demand_MW / 1000,
          Solar     = Solar_MW / 1000,
          Wind      = Wind_MW / 1000,
          Renewable = Renewable_MW / 1000
        )
      x_title <- "Timestamp (Last 14 Days of Selection)"
    } else {
      plot_data <- df %>%
        transmute(
          Timestamp,
          Demand    = Demand_MW / 1000,
          Solar     = Solar_MW / 1000,
          Wind      = Wind_MW / 1000,
          Renewable = Renewable_MW / 1000
        )
      x_title <- "Timestamp"
    }
    x_var <- plot_data$Timestamp
  }
  
  p <- plot_ly(data = plot_data, x = x_var) %>%
    add_lines(y = ~Demand, name = "Electricity Demand", 
              line = list(color = energy_colors$demand, width = 2)) %>%
    add_lines(y = ~Renewable, name = "Total Renewable (Solar+Wind)", 
              line = list(color = energy_colors$renewable, width = 2)) %>%
    add_lines(y = ~Solar, name = "Solar Generation", 
              line = list(color = energy_colors$solar, width = 1.5, dash = "dash")) %>%
    add_lines(y = ~Wind, name = "Wind Generation", 
              line = list(color = energy_colors$wind, width = 1.5, dash = "dot")) %>%
    layout(
      title = list(text = "<b>Grid Electricity Demand vs. Renewable Generation</b>", font = list(size = 14)),
      xaxis = list(title = x_title, showgrid = TRUE, gridcolor = energy_colors$grid),
      yaxis = list(title = "Power (Gigawatts - GW)", showgrid = TRUE, gridcolor = energy_colors$grid),
      hovermode = "x unified",
      legend = list(orientation = "h", x = 0.05, y = 1.12),
      margin = list(t = 60, b = 40, l = 60, r = 20)
    )
  
  return(p)
}

#' Diurnal 24-Hour Profile for Solar and Wind
plot_diurnal_generation_profile <- function(df) {
  if (nrow(df) == 0) return(plotly_empty())
  
  diurnal <- df %>%
    group_by(Hour) %>%
    summarise(
      Solar_Mean = mean(Solar_MW, na.rm = TRUE) / 1000,
      Solar_P10  = quantile(Solar_MW, 0.10, na.rm = TRUE) / 1000,
      Solar_P90  = quantile(Solar_MW, 0.90, na.rm = TRUE) / 1000,
      Wind_Mean  = mean(Wind_MW, na.rm = TRUE) / 1000,
      Wind_P10   = quantile(Wind_MW, 0.10, na.rm = TRUE) / 1000,
      Wind_P90   = quantile(Wind_MW, 0.90, na.rm = TRUE) / 1000,
      Demand_Mean= mean(Demand_MW, na.rm = TRUE) / 1000,
      .groups = "drop"
    )
  
  p <- plot_ly(data = diurnal, x = ~Hour) %>%
    # Solar Ribbon (P10 - P90)
    add_ribbons(ymin = ~Solar_P10, ymax = ~Solar_P90, name = "Solar 10th-90th Pct",
                fillcolor = "rgba(245, 158, 11, 0.18)", line = list(color = "transparent"),
                showlegend = TRUE) %>%
    add_lines(y = ~Solar_Mean, name = "Solar Mean (GW)",
              line = list(color = energy_colors$solar, width = 3)) %>%
    # Wind Ribbon (P10 - P90)
    add_ribbons(ymin = ~Wind_P10, ymax = ~Wind_P90, name = "Wind 10th-90th Pct",
                fillcolor = "rgba(2, 132, 199, 0.18)", line = list(color = "transparent"),
                showlegend = TRUE) %>%
    add_lines(y = ~Wind_Mean, name = "Wind Mean (GW)",
              line = list(color = energy_colors$wind, width = 3)) %>%
    layout(
      title = list(text = "<b>24-Hour Diurnal Generation Profile (Solar vs. Wind)</b>", font = list(size = 14)),
      xaxis = list(title = "Hour of Day (0 - 23 IST)", dtick = 2, showgrid = TRUE),
      yaxis = list(title = "Generation (Gigawatts - GW)", showgrid = TRUE),
      hovermode = "x unified",
      legend = list(orientation = "h", x = 0.05, y = 1.12),
      margin = list(t = 60, b = 40, l = 60, r = 20)
    )
  
  return(p)
}

#' Seasonal Generation Distribution (Boxplots or Grouped Bars)
plot_seasonal_monthly_generation <- function(df, view_by = "Season") {
  if (nrow(df) == 0) return(plotly_empty())
  
  if (view_by == "Season") {
    summary_df <- df %>%
      group_by(Season) %>%
      summarise(
        Solar_TWh  = round(sum(Solar_MW, na.rm = TRUE) / 1e6, 2),
        Wind_TWh   = round(sum(Wind_MW, na.rm = TRUE) / 1e6, 2),
        Demand_TWh = round(sum(Demand_MW, na.rm = TRUE) / 1e6, 2),
        Avg_Solar_GW = round(mean(Solar_MW, na.rm = TRUE) / 1000, 2),
        Avg_Wind_GW  = round(mean(Wind_MW, na.rm = TRUE) / 1000, 2),
        .groups = "drop"
      )
    
    p <- plot_ly(data = summary_df, x = ~Season) %>%
      add_bars(y = ~Solar_TWh, name = "Solar Energy (TWh)", marker = list(color = energy_colors$solar)) %>%
      add_bars(y = ~Wind_TWh, name = "Wind Energy (TWh)", marker = list(color = energy_colors$wind)) %>%
      layout(
        title = list(text = "<b>Total Energy Generation by Season</b>", font = list(size = 14)),
        xaxis = list(title = "Season"),
        yaxis = list(title = "Energy (Terawatt-hours - TWh)"),
        barmode = "group",
        legend = list(orientation = "h", x = 0.2, y = 1.1),
        margin = list(t = 50, b = 40, l = 60, r = 20)
      )
  } else {
    monthly_df <- df %>%
      group_by(Year, Month) %>%
      summarise(
        Solar_TWh = round(sum(Solar_MW, na.rm = TRUE) / 1e6, 2),
        Wind_TWh  = round(sum(Wind_MW, na.rm = TRUE) / 1e6, 2),
        YearMonth = paste(first(Month), first(Year)),
        .groups = "drop"
      )
    
    p <- plot_ly(data = monthly_df, x = ~YearMonth) %>%
      add_bars(y = ~Solar_TWh, name = "Solar (TWh)", marker = list(color = energy_colors$solar)) %>%
      add_bars(y = ~Wind_TWh, name = "Wind (TWh)", marker = list(color = energy_colors$wind)) %>%
      layout(
        title = list(text = "<b>Monthly Renewable Energy Generation</b>", font = list(size = 14)),
        xaxis = list(title = "Month - Year", categoryorder = "trace"),
        yaxis = list(title = "Energy (TWh)"),
        barmode = "stack",
        legend = list(orientation = "h", x = 0.2, y = 1.1),
        margin = list(t = 50, b = 50, l = 60, r = 20)
      )
  }
  
  return(p)
}

#' Generation Duration Curve (sorted load duration for Solar and Wind)
plot_duration_curve <- function(df) {
  if (nrow(df) == 0) return(plotly_empty())
  
  n_pts <- nrow(df)
  pct_time <- seq(0, 100, length.out = min(1000, n_pts))
  
  # Sample sorted percentiles for smooth rendering
  sol_sorted <- quantile(df$Solar_MW / 1000, probs = seq(1, 0, length.out = length(pct_time)), na.rm = TRUE)
  wnd_sorted <- quantile(df$Wind_MW / 1000, probs = seq(1, 0, length.out = length(pct_time)), na.rm = TRUE)
  dem_sorted <- quantile(df$Demand_MW / 1000, probs = seq(1, 0, length.out = length(pct_time)), na.rm = TRUE)
  net_sorted <- quantile(df$Residual_Demand_MW / 1000, probs = seq(1, 0, length.out = length(pct_time)), na.rm = TRUE)
  
  dur_df <- data.frame(
    Percent_Time = pct_time,
    Solar = sol_sorted,
    Wind = wnd_sorted,
    Demand = dem_sorted,
    Net_Demand = net_sorted
  )
  
  p <- plot_ly(data = dur_df, x = ~Percent_Time) %>%
    add_lines(y = ~Demand, name = "Gross Demand", line = list(color = energy_colors$demand, width = 2)) %>%
    add_lines(y = ~Net_Demand, name = "Net Demand (Residual)", line = list(color = energy_colors$net_load, width = 2, dash = "dot")) %>%
    add_lines(y = ~Solar, name = "Solar Generation", line = list(color = energy_colors$solar, width = 1.8)) %>%
    add_lines(y = ~Wind, name = "Wind Generation", line = list(color = energy_colors$wind, width = 1.8)) %>%
    layout(
      title = list(text = "<b>Load & Generation Duration Curves</b>", font = list(size = 14)),
      xaxis = list(title = "Percentage of Time Exceeded (%)", dtick = 10, range = c(0, 100)),
      yaxis = list(title = "Power (GW)"),
      hovermode = "x unified",
      legend = list(orientation = "h", x = 0.05, y = 1.12),
      margin = list(t = 60, b = 40, l = 60, r = 20)
    )
  
  return(p)
}

#' The Indian Grid Duck Curve: Hourly Demand vs Net Demand (Residual Load)
plot_duck_curve <- function(df, season_filter = "All") {
  if (nrow(df) == 0) return(plotly_empty())
  
  sub_df <- df
  if (season_filter != "All") {
    sub_df <- sub_df %>% filter(Season == season_filter)
  }
  
  duck_data <- sub_df %>%
    group_by(Hour) %>%
    summarise(
      Gross_Demand_GW = mean(Demand_MW, na.rm = TRUE) / 1000,
      Solar_GW        = mean(Solar_MW, na.rm = TRUE) / 1000,
      Renewable_GW    = mean(Renewable_MW, na.rm = TRUE) / 1000,
      Net_Demand_GW   = mean(Residual_Demand_MW, na.rm = TRUE) / 1000,
      .groups = "drop"
    )
  
  p <- plot_ly(data = duck_data, x = ~Hour) %>%
    add_lines(y = ~Gross_Demand_GW, name = "Gross Demand (GW)",
              line = list(color = energy_colors$demand, width = 2.5)) %>%
    add_lines(y = ~Net_Demand_GW, name = "Net Demand / Residual Load (GW)",
              line = list(color = energy_colors$net_load, width = 3)) %>%
    add_lines(y = ~Renewable_GW, name = "Renewable Generation (GW)",
              line = list(color = energy_colors$renewable, width = 2, dash = "dash")) %>%
    layout(
      title = list(text = paste0("<b>The Indian Grid Duck Curve (", season_filter, ")</b>"), font = list(size = 14)),
      xaxis = list(title = "Hour of Day (0 - 23 IST)", dtick = 2, showgrid = TRUE),
      yaxis = list(title = "Power (Gigawatts - GW)", showgrid = TRUE),
      hovermode = "x unified",
      legend = list(orientation = "h", x = 0.05, y = 1.12),
      annotations = list(
        list(
          x = 12, y = duck_data$Net_Demand_GW[duck_data$Hour == 12],
          text = "Solar 'Belly' (Mid-day dip)",
          showarrow = TRUE, arrowhead = 2, ax = 0, ay = -35,
          font = list(color = energy_colors$solar, size = 11)
        ),
        list(
          x = 19, y = duck_data$Net_Demand_GW[duck_data$Hour == 19],
          text = "Evening 'Neck' Ramp (Steep climb)",
          showarrow = TRUE, arrowhead = 2, ax = 30, ay = -30,
          font = list(color = energy_colors$danger, size = 11)
        )
      ),
      margin = list(t = 60, b = 40, l = 60, r = 20)
    )
  
  return(p)
}

#' Hourly Ramping Rate Distribution
plot_ramp_rate_distribution <- function(df) {
  if (nrow(df) == 0) return(plotly_empty())
  
  p <- plot_ly(x = df$Net_Ramp_MW / 1000, type = "histogram",
               marker = list(color = energy_colors$net_load, line = list(color = "#4C1D95", width = 0.5)),
               nbinsx = 50) %>%
    layout(
      title = list(text = "<b>Distribution of 1-Hour Net Load Ramp Rates</b>", font = list(size = 14)),
      xaxis = list(title = "Hourly Net Ramp (GW / hour) [Positive = Ramping Up, Negative = Ramping Down]"),
      yaxis = list(title = "Frequency (Hours)"),
      margin = list(t = 50, b = 40, l = 60, r = 20)
    )
  
  return(p)
}

#' 2D Hour-of-Day vs Month Operational Heatmap
plot_operational_heatmap <- function(df, metric = "Solar_MW") {
  if (nrow(df) == 0) return(plotly_empty())
  
  metric_name <- switch(metric,
    "Solar_MW"            = "Average Solar Generation (GW)",
    "Wind_MW"             = "Average Wind Generation (GW)",
    "Demand_MW"           = "Average Electricity Demand (GW)",
    "Renewable_Share_Pct" = "Renewable Penetration (%)",
    "Residual_Demand_MW"  = "Residual Demand (GW)"
  )
  
  heat_df <- df %>%
    group_by(Month, Hour) %>%
    summarise(
      Val = mean(.data[[metric]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Convert MW to GW for power metrics
  if (grepl("MW", metric)) {
    heat_df$Val <- round(heat_df$Val / 1000, 2)
  } else {
    heat_df$Val <- round(heat_df$Val, 2)
  }
  
  # Pivot to matrix for heatmap
  matrix_df <- heat_df %>%
    pivot_wider(names_from = Month, values_from = Val)
  
  hours <- matrix_df$Hour
  months <- setdiff(colnames(matrix_df), "Hour")
  z_vals <- as.matrix(matrix_df[, months])
  
  color_scale <- if (metric == "Solar_MW") {
    "YlOrRd"
  } else if (metric == "Wind_MW") {
    "Blues"
  } else if (metric == "Renewable_Share_Pct") {
    "Viridis"
  } else {
    "Plasma"
  }
  
  p <- plot_ly(
    x = months,
    y = hours,
    z = z_vals,
    type = "heatmap",
    colorscale = color_scale,
    colorbar = list(title = if (grepl("MW", metric)) "GW" else "%")
  ) %>%
    layout(
      title = list(text = paste0("<b>Diurnal-Seasonal Heatmap: ", metric_name, "</b>"), font = list(size = 14)),
      xaxis = list(title = "Month"),
      yaxis = list(title = "Hour of Day (0 - 23 IST)", dtick = 2),
      margin = list(t = 50, b = 40, l = 60, r = 20)
    )
  
  return(p)
}

# ------------------------------------------------------------------------------
# 3. Forecasting Engine (Holt-Winters / Exponential Smoothing / Linear Harmonic)
# ------------------------------------------------------------------------------

#' Run chronological holdout forecast evaluation & future projection
#'
#' @param df Data frame
#' @param target Target variable: "Demand_MW", "Solar_MW", "Wind_MW", "Renewable_MW"
#' @param horizon Days to forecast (e.g. 7, 14, 30)
#' @param test_split Fraction of historical data for test holdout (default 0.15 = 15%)
#' @return List containing historical data, holdout predictions, future forecast, and accuracy metrics
run_time_series_forecast <- function(df, target = "Demand_MW", horizon = 14, test_split = 0.15) {
  # Aggregate to daily metrics for robust, explainable forecasting
  daily_df <- df %>%
    group_by(Date) %>%
    summarise(
      Actual = mean(.data[[target]], na.rm = TRUE) / 1000, # In GW
      Peak   = max(.data[[target]], na.rm = TRUE) / 1000,
      .groups = "drop"
    ) %>%
    arrange(Date) %>%
    filter(!is.na(Actual))
  
  n_total <- nrow(daily_df)
  if (n_total < 30) {
    stop("Insufficient data points for forecasting (minimum 30 daily observations required).")
  }
  
  n_test  <- max(7, round(n_total * test_split))
  n_train <- n_total - n_test
  
  train_df <- daily_df[1:n_train, ]
  test_df  <- daily_df[(n_train + 1):n_total, ]
  
  # Fit Holt-Winters or Exponential Smoothing time series with weekly seasonality (frequency = 7)
  train_ts <- ts(train_df$Actual, frequency = 7)
  
  # Robust fitting: try additive seasonal; if optimization complains or fails, fall back to non-seasonal
  fit_hw <- suppressWarnings(tryCatch({
    HoltWinters(train_ts, seasonal = "additive")
  }, error = function(e) {
    tryCatch({
      HoltWinters(train_ts, gamma = FALSE) # Non-seasonal Holt
    }, error = function(e2) {
      HoltWinters(train_ts, beta = FALSE, gamma = FALSE) # Simple Exponential
    })
  }))
  
  # Predict test holdout
  pred_test_obj <- suppressWarnings(predict(fit_hw, n.ahead = n_test, prediction.interval = TRUE, level = 0.95))
  pred_test <- as.numeric(pred_test_obj[, "fit"])
  pred_test_lwr <- as.numeric(pred_test_obj[, "lwr"])
  pred_test_upr <- as.numeric(pred_test_obj[, "upr"])
  
  # Clamp negative forecasts to 0 for solar/wind
  if (target %in% c("Solar_MW", "Wind_MW", "Renewable_MW")) {
    pred_test <- pmax(0, pred_test)
    pred_test_lwr <- pmax(0, pred_test_lwr)
    pred_test_upr <- pmax(0, pred_test_upr)
  }
  
  # Calculate Accuracy Metrics on Holdout Set (Chronological test set, no data leakage!)
  actual_test <- test_df$Actual
  mae  <- round(mean(abs(actual_test - pred_test), na.rm = TRUE), 3)
  rmse <- round(sqrt(mean((actual_test - pred_test)^2, na.rm = TRUE)), 3)
  mape <- round(mean(abs((actual_test - pred_test) / actual_test), na.rm = TRUE) * 100, 2)
  coverage_95 <- round(mean(actual_test >= pred_test_lwr & actual_test <= pred_test_upr, na.rm = TRUE) * 100, 1)
  
  # Now fit on ALL available data to generate future forecast
  all_ts <- ts(daily_df$Actual, frequency = 7)
  fit_all <- suppressWarnings(tryCatch({
    HoltWinters(all_ts, seasonal = "additive")
  }, error = function(e) {
    tryCatch({
      HoltWinters(all_ts, gamma = FALSE)
    }, error = function(e2) {
      HoltWinters(all_ts, beta = FALSE, gamma = FALSE)
    })
  }))
  
  pred_future_obj <- suppressWarnings(predict(fit_all, n.ahead = horizon, prediction.interval = TRUE, level = 0.95))
  
  future_dates <- seq(max(daily_df$Date) + 1, by = "day", length.out = horizon)
  future_fit <- as.numeric(pred_future_obj[, "fit"])
  future_lwr <- as.numeric(pred_future_obj[, "lwr"])
  future_upr <- as.numeric(pred_future_obj[, "upr"])
  
  if (target %in% c("Solar_MW", "Wind_MW", "Renewable_MW")) {
    future_fit <- pmax(0, future_fit)
    future_lwr <- pmax(0, future_lwr)
    future_upr <- pmax(0, future_upr)
  }
  
  future_df <- data.frame(
    Date     = future_dates,
    Forecast = round(future_fit, 2),
    Lower_95 = round(future_lwr, 2),
    Upper_95 = round(future_upr, 2)
  )
  
  test_results_df <- data.frame(
    Date        = test_df$Date,
    Actual      = round(actual_test, 2),
    Predicted   = round(pred_test, 2),
    Lower_Bound = round(pred_test_lwr, 2),
    Upper_Bound = round(pred_test_upr, 2),
    Error       = round(actual_test - pred_test, 2)
  )
  
  list(
    model_name      = "Holt-Winters Triple Exponential Smoothing (Weekly Periodicity)",
    target_variable = target,
    horizon_days    = horizon,
    metrics         = list(
      MAE         = mae,
      RMSE        = rmse,
      MAPE        = mape,
      Coverage_95 = coverage_95
    ),
    train_dates     = c(min(train_df$Date), max(train_df$Date)),
    test_dates      = c(min(test_df$Date), max(test_df$Date)),
    historical_df   = daily_df,
    test_results_df = test_results_df,
    future_df       = future_df
  )
}

#' Plot Forecast Results with Uncertainty Bands and Holdout Comparison
plot_forecast_results <- function(forecast_res) {
  hist_df <- forecast_res$historical_df
  test_df <- forecast_res$test_results_df
  fut_df  <- forecast_res$future_df
  
  target_label <- switch(forecast_res$target_variable,
    "Demand_MW"    = "Electricity Demand (GW)",
    "Solar_MW"     = "Solar Generation (GW)",
    "Wind_MW"      = "Wind Generation (GW)",
    "Renewable_MW" = "Total Renewable (GW)"
  )
  
  # Only plot recent 90 days of history for clear visual contrast
  hist_recent <- tail(hist_df, 90)
  
  p <- plot_ly() %>%
    # Historical Actual
    add_lines(data = hist_recent, x = ~Date, y = ~Actual, name = "Historical Observed",
              line = list(color = "#334155", width = 2)) %>%
    # Holdout Test Prediction
    add_lines(data = test_df, x = ~Date, y = ~Predicted, name = "Holdout Test Prediction",
              line = list(color = "#0284C7", width = 2, dash = "dash")) %>%
    # Future Confidence Band (80-95%)
    add_ribbons(data = fut_df, x = ~Date, ymin = ~Lower_95, ymax = ~Upper_95,
                name = "95% Prediction Interval",
                fillcolor = "rgba(16, 185, 129, 0.2)", line = list(color = "transparent")) %>%
    # Future Forecast Line
    add_lines(data = fut_df, x = ~Date, y = ~Forecast, name = "Future Forecast",
              line = list(color = "#10B981", width = 2.5)) %>%
    layout(
      title = list(text = paste0("<b>Time-Series Forecast: ", target_label, "</b>"), font = list(size = 14)),
      xaxis = list(title = "Date", showgrid = TRUE),
      yaxis = list(title = target_label, showgrid = TRUE),
      hovermode = "x unified",
      legend = list(orientation = "h", x = 0.05, y = 1.12),
      margin = list(t = 60, b = 40, l = 60, r = 20)
    )
  
  return(p)
}

# ------------------------------------------------------------------------------
# 4. Data-Driven Business Insights Generator
# ------------------------------------------------------------------------------

#' Compute dynamic, fact-based business insights and operational recommendations
#'
#' @param df Cleaned data frame
#' @return List containing computed findings and actionable recommendations
generate_business_insights <- function(df) {
  if (nrow(df) == 0) return(list())
  
  # 1. Peak Solar Window Analysis
  solar_by_hour <- df %>%
    group_by(Hour) %>%
    summarise(
      Mean_Solar_GW = mean(Solar_MW, na.rm = TRUE) / 1000,
      Mean_Share    = mean(Solar_Share_Pct, na.rm = TRUE),
      .groups = "drop"
    )
  peak_solar_hour <- solar_by_hour$Hour[which.max(solar_by_hour$Mean_Solar_GW)]
  peak_solar_mean <- max(solar_by_hour$Mean_Solar_GW)
  peak_solar_share <- max(solar_by_hour$Mean_Share)
  
  # 2. Evening Ramp Rate Crunch (17:00 to 20:00)
  ramp_window <- df %>%
    filter(Hour %in% c(17, 18, 19, 20)) %>%
    group_by(Date) %>%
    summarise(
      Ramp_17_20 = last(Residual_Demand_MW) - first(Residual_Demand_MW),
      .groups = "drop"
    )
  avg_evening_ramp_gw <- mean(ramp_window$Ramp_17_20, na.rm = TRUE) / 1000
  max_evening_ramp_gw <- max(ramp_window$Ramp_17_20, na.rm = TRUE) / 1000
  
  # 3. Seasonal Wind vs Solar Complementarity
  season_stats <- df %>%
    group_by(Season) %>%
    summarise(
      Avg_Solar_GW = mean(Solar_MW, na.rm = TRUE) / 1000,
      Avg_Wind_GW  = mean(Wind_MW, na.rm = TRUE) / 1000,
      Avg_Demand_GW= mean(Demand_MW, na.rm = TRUE) / 1000,
      .groups = "drop"
    )
  
  monsoon_wind <- season_stats$Avg_Wind_GW[season_stats$Season == "Monsoon"]
  winter_wind  <- season_stats$Avg_Wind_GW[season_stats$Season == "Winter"]
  summer_solar <- season_stats$Avg_Solar_GW[season_stats$Season == "Summer / Pre-Monsoon"]
  monsoon_solar<- season_stats$Avg_Solar_GW[season_stats$Season == "Monsoon"]
  
  wind_monsoon_boost_pct <- if (length(winter_wind) > 0 && winter_wind > 0) {
    round(((monsoon_wind - winter_wind) / winter_wind) * 100, 1)
  } else 0
  
  solar_monsoon_drop_pct <- if (length(summer_solar) > 0 && summer_solar > 0) {
    round(((summer_solar - monsoon_solar) / summer_solar) * 100, 1)
  } else 0
  
  # 4. Critical RE Shortfall Periods
  crit_hours <- sum(df$Renewable_Share_Pct < 5, na.rm = TRUE)
  crit_pct   <- round((crit_hours / nrow(df)) * 100, 1)
  
  # 5. Correlation between Solar and Wind
  cor_sw <- round(cor(df$Solar_MW, df$Wind_MW, use = "complete.obs"), 3)
  
  list(
    peak_solar_hour        = peak_solar_hour,
    peak_solar_mean_gw     = round(peak_solar_mean, 2),
    peak_solar_share_pct   = round(peak_solar_share, 1),
    avg_evening_ramp_gw    = round(avg_evening_ramp_gw, 2),
    max_evening_ramp_gw    = round(max_evening_ramp_gw, 2),
    wind_monsoon_boost_pct = wind_monsoon_boost_pct,
    solar_monsoon_drop_pct = solar_monsoon_drop_pct,
    crit_shortfall_hours   = crit_hours,
    crit_shortfall_pct     = crit_pct,
    solar_wind_correlation = cor_sw,
    season_stats           = season_stats
  )
}
