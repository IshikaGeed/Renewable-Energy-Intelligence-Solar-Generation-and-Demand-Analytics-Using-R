# ==============================================================================
# Renewable Energy Intelligence: Solar, Wind & Demand Analytics
# Business Intelligence Case Study Using R & Shiny
# Framework: Shiny + bslib (Bootstrap 5) + plotly + DT + dplyr
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(scales)
})

# Source modular R code
source("R/data_processing.R")
source("R/analysis.R")

# Preload cleaned data
init_df <- load_clean_data()
min_date <- min(init_df$Date)
max_date <- max(init_df$Date)

# Custom Theme definition (Clean, modern system font stack for instant offline rendering)
app_theme <- bs_theme(
  version = 5,
  bootswatch = "zephyr",
  primary = "#0F766E",     # Deep Teal / Emerald
  secondary = "#0284C7",   # Sky Blue
  success = "#10B981",     # Green
  warning = "#F59E0B",     # Amber
  danger = "#EF4444",      # Crimson
  info = "#0EA5E9",
  bg = "#F8FAFC",          # Crisp light neutral
  fg = "#0F172A"           # Slate text
)

# Custom CSS
custom_css <- "
  .navbar { box-shadow: 0 2px 4px rgba(0,0,0,0.06); }
  .card { border-radius: 8px; border: 1px solid #E2E8F0; box-shadow: 0 1px 3px rgba(0,0,0,0.03); }
  .value-box { border-radius: 8px; }
  .nav-pills .nav-link.active { background-color: #0F766E !important; }
  .badge-tag { font-size: 0.8rem; padding: 4px 8px; border-radius: 4px; font-weight: 600; }
  .sidebar-filter-box { background: #FFFFFF; padding: 15px; border-radius: 8px; border: 1px solid #E2E8F0; }
  .rec-card { border-left: 4px solid #0F766E; background: #FFFFFF; padding: 16px; margin-bottom: 12px; border-radius: 4px; box-shadow: 0 1px 2px rgba(0,0,0,0.04); }
  .rec-priority-high { border-left-color: #EF4444; }
  .rec-priority-med { border-left-color: #F59E0B; }
  .rec-priority-strategic { border-left-color: #0284C7; }
"

# ------------------------------------------------------------------------------
# UI DEFINITION
# ------------------------------------------------------------------------------
ui <- page_navbar(
  title = span(
    tags$span(style = "color: #F59E0B; margin-right: 6px;", "⚡"),
    "Renewable Energy Intelligence",
    tags$span(style = "font-size: 0.75rem; font-weight: normal; color: #64748B; margin-left: 8px;", "India Grid Analytics")
  ),
  theme = app_theme,
  header = tags$head(tags$style(HTML(custom_css))),
  
  # Sidebar Filter Drawer (Available across relevant tabs)
  sidebar = sidebar(
    title = "Filters & Controls",
    width = 300,
    
    dateRangeInput(
      inputId = "date_range",
      label = "Date Range:",
      start = min_date,
      end = max_date,
      min = min_date,
      max = max_date,
      format = "yyyy-mm-dd",
      separator = " to "
    ),
    
    div(
      class = "mb-3",
      tags$label(class = "control-label mb-1", "Quick Date Presets:"),
      div(
        class = "btn-group btn-group-sm w-100",
        actionButton("btn_all", "All", class = "btn-outline-secondary"),
        actionButton("btn_2024", "2024", class = "btn-outline-secondary"),
        actionButton("btn_2025", "2025", class = "btn-outline-secondary"),
        actionButton("btn_30d", "Last 30D", class = "btn-outline-secondary")
      )
    ),
    
    checkboxGroupInput(
      inputId = "season_filter",
      label = "Seasons:",
      choices = c("Winter", "Summer / Pre-Monsoon", "Monsoon", "Post-Monsoon"),
      selected = c("Winter", "Summer / Pre-Monsoon", "Monsoon", "Post-Monsoon")
    ),
    
    radioButtons(
      inputId = "day_type_filter",
      label = "Day Type:",
      choices = c("All Days" = "all", "Weekdays" = "weekday", "Weekends" = "weekend"),
      selected = "all",
      inline = TRUE
    ),
    
    hr(),
    div(
      style = "font-size: 0.8rem; color: #64748B;",
      tags$b("Data Source:"), br(),
      "Grid Controller of India (Grid-India / NERLDC)", br(),
      "Ministry of Power, Govt. of India", br(),
      "Coverage: Jan 2024 – Jun 2025 (1-hr interval)"
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 1: OVERVIEW
  # ----------------------------------------------------------------------------
  nav_panel(
    title = "Overview",
    
    layout_columns(
      fill = FALSE,
      value_box(
        title = "Total Clean Energy Generated",
        value = textOutput("kpi_total_re"),
        showcase = bsicons::bs_icon("sun-fill", size = "2em", class = "text-warning"),
        theme = "light",
        p(textOutput("kpi_re_share_sub"), class = "text-muted mb-0")
      ),
      value_box(
        title = "Solar Generation",
        value = textOutput("kpi_solar_twh"),
        showcase = bsicons::bs_icon("brightness-high", size = "2em", class = "text-warning"),
        theme = "light",
        p(textOutput("kpi_solar_peak_sub"), class = "text-muted mb-0")
      ),
      value_box(
        title = "Wind Generation",
        value = textOutput("kpi_wind_twh"),
        showcase = bsicons::bs_icon("wind", size = "2em", class = "text-info"),
        theme = "light",
        p(textOutput("kpi_wind_peak_sub"), class = "text-muted mb-0")
      ),
      value_box(
        title = "Total Electricity Demand",
        value = textOutput("kpi_demand_twh"),
        showcase = bsicons::bs_icon("lightning-charge-fill", size = "2em", class = "text-primary"),
        theme = "light",
        p(textOutput("kpi_demand_peak_sub"), class = "text-muted mb-0")
      )
    ),
    
    layout_columns(
      fill = FALSE,
      value_box(
        title = "Average Demand (GW)",
        value = textOutput("kpi_avg_demand"),
        showcase = bsicons::bs_icon("speedometer2", size = "1.5em"),
        theme = "light"
      ),
      value_box(
        title = "Max Instantaneous RE Share",
        value = textOutput("kpi_max_re_share"),
        showcase = bsicons::bs_icon("pie-chart-fill", size = "1.5em", class = "text-success"),
        theme = "light"
      ),
      value_box(
        title = "Max 1-Hour Net Ramp",
        value = textOutput("kpi_max_ramp"),
        showcase = bsicons::bs_icon("graph-up-arrow", size = "1.5em", class = "text-danger"),
        theme = "light"
      ),
      value_box(
        title = "Critical RE Shortfall Hours (<5%)",
        value = textOutput("kpi_crit_shortfall"),
        showcase = bsicons::bs_icon("exclamation-triangle-fill", size = "1.5em", class = "text-warning"),
        theme = "light"
      )
    ),
    
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        span("Electricity Demand & Renewable Generation Timeline"),
        radioButtons("overview_agg", NULL, 
                     choices = c("Daily" = "daily", "Weekly" = "weekly", "Hourly" = "hourly"), 
                     selected = "daily", inline = TRUE)
      ),
      card_body(
        plotlyOutput("plot_overview_timeline", height = "380px")
      )
    ),
    
    card(
      card_header("Executive Summary & Current Scope"),
      card_body(
        uiOutput("overview_summary_text")
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 2: GENERATION ANALYSIS
  # ----------------------------------------------------------------------------
  nav_panel(
    title = "Generation Analysis",
    
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Diurnal 24-Hour Profile: Solar & Wind Generation"),
        card_body(
          plotlyOutput("plot_diurnal_profile", height = "380px"),
          p(class = "text-muted small mt-2 mb-0",
            "Ribbons represent the 10th to 90th percentile distribution across all days. Notice Solar's consistent bell curve centered at 12:00-13:00 IST, contrasted with Wind's nocturnal and afternoon patterns.")
        )
      ),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Seasonal / Monthly Generation"),
          radioButtons("seasonal_view_mode", NULL, 
                       choices = c("By Season" = "Season", "By Month" = "Month"), 
                       selected = "Season", inline = TRUE)
        ),
        card_body(
          plotlyOutput("plot_seasonal_gen", height = "380px")
        )
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Load & Generation Duration Curves"),
        card_body(
          plotlyOutput("plot_duration", height = "350px"),
          p(class = "text-muted small mt-2 mb-0",
            "Sorted load duration curves illustrate capacity utilization and the percentage of hours in the period where generation exceeds a given threshold.")
        )
      ),
      card(
        card_header("Historical Generation Peak & Low Milestones"),
        card_body(
          DTOutput("table_generation_milestones")
        )
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 3: GENERATION VS DEMAND (THE DUCK CURVE)
  # ----------------------------------------------------------------------------
  nav_panel(
    title = "Generation vs Demand",
    
    div(
      class = "alert alert-info py-2 px-3 mb-3",
      tags$b("Grid Net Load Definition: "),
      "Residual Demand (Net Load) = Electricity Demand minus (Solar + Wind). ",
      "A net demand gap does not imply a power shortage or blackout; it represents the net demand that must be balanced by dispatchable thermal, hydro, nuclear, storage, and inter-regional power transfers."
    ),
    
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span("The Indian Grid Duck Curve"),
          selectInput("duck_season", NULL, 
                      choices = c("All Seasons" = "All", "Winter", "Summer / Pre-Monsoon", "Monsoon", "Post-Monsoon"),
                      selected = "All", width = "200px")
        ),
        card_body(
          plotlyOutput("plot_duck_curve", height = "380px"),
          p(class = "text-muted small mt-2 mb-0",
            "Observe the deep mid-day 'belly' carved out by high solar irradiance, followed by the steep evening 'neck' ramp between 17:00 and 20:00 IST as solar vanishes and lighting/cooling demand surges.")
        )
      ),
      card(
        card_header("1-Hour Net Load Ramp Rate Distribution"),
        card_body(
          plotlyOutput("plot_ramp_dist", height = "380px"),
          p(class = "text-muted small mt-2 mb-0",
            "Positive values represent hours where dispatchable generators must ramp UP. Negative values represent hours where dispatchable generators must ramp DOWN.")
        )
      )
    ),
    
    card(
      card_header("Renewable Penetration Distribution (% of Hourly Demand Met)"),
      card_body(
        plotlyOutput("plot_penetration_dist", height = "280px")
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 4: PERFORMANCE & FLEXIBILITY
  # ----------------------------------------------------------------------------
  nav_panel(
    title = "Energy Performance",
    
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Diurnal-Seasonal 2D Heatmap"),
          selectInput("heatmap_metric", NULL,
                      choices = c("Solar Generation" = "Solar_MW",
                                  "Wind Generation" = "Wind_MW",
                                  "Electricity Demand" = "Demand_MW",
                                  "Renewable Share %" = "Renewable_Share_Pct",
                                  "Residual Net Demand" = "Residual_Demand_MW"),
                      selected = "Solar_MW", width = "220px")
        ),
        card_body(
          plotlyOutput("plot_operational_heatmap", height = "400px")
        )
      ),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Correlation & Scatter Analysis"),
          selectInput("scatter_pair", NULL,
                      choices = c("Solar vs Wind" = "solar_wind",
                                  "Demand vs Solar" = "demand_solar",
                                  "Demand vs Wind" = "demand_wind",
                                  "Demand vs Renewable" = "demand_re"),
                      selected = "solar_wind", width = "200px")
        ),
        card_body(
          plotlyOutput("plot_correlation_scatter", height = "340px"),
          uiOutput("scatter_stats_text")
        )
      )
    ),
    
    card(
      card_header("System Flexibility & Reliability Performance Table"),
      card_body(
        tableOutput("table_performance_metrics")
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 5: FORECASTING
  # ----------------------------------------------------------------------------
  nav_panel(
    title = "Forecasting",
    
    layout_columns(
      col_widths = c(3, 9),
      card(
        card_header("Forecast Parameters"),
        card_body(
          selectInput("forecast_target", "Target Series:",
                      choices = c("Electricity Demand" = "Demand_MW",
                                  "Solar Generation" = "Solar_MW",
                                  "Wind Generation" = "Wind_MW",
                                  "Total Renewable" = "Renewable_MW"),
                      selected = "Demand_MW"),
          sliderInput("forecast_horizon", "Forecast Horizon (Days):",
                      min = 7, max = 30, value = 14, step = 7),
          sliderInput("forecast_holdout", "Holdout Validation Split:",
                      min = 0.10, max = 0.25, value = 0.15, step = 0.05),
          actionButton("btn_run_forecast", "Update Forecast", class = "btn-primary w-100 mt-2"),
          hr(),
          p(class = "text-muted small",
            tags$b("Model Architecture:"), br(),
            "Holt-Winters Exponential Smoothing with weekly seasonality (frequency = 7 days).", br(),
            "Evaluated strictly on an out-of-sample chronological holdout set to prevent data leakage.")
        )
      ),
      card(
        card_header("Holdout Validation & Out-of-Sample Forecast"),
        card_body(
          layout_columns(
            fill = FALSE,
            value_box("Holdout MAPE (%)", textOutput("fc_mape"), theme = "light"),
            value_box("Holdout RMSE (GW)", textOutput("fc_rmse"), theme = "light"),
            value_box("Holdout MAE (GW)", textOutput("fc_mae"), theme = "light"),
            value_box("95% Interval Coverage", textOutput("fc_coverage"), theme = "light")
          ),
          plotlyOutput("plot_forecast", height = "360px"),
          p(class = "text-muted small mt-2 mb-0",
            "The model is trained on the first portion of historical data, tested against the out-of-sample holdout test set (dashed blue), and then refit on full data to project future horizons with 95% confidence intervals (green).")
        )
      )
    ),
    
    card(
      card_header("Future Projections Table (Daily Values)"),
      card_body(
        DTOutput("table_future_forecast")
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 6: BUSINESS INSIGHTS & RECOMMENDATIONS
  # ----------------------------------------------------------------------------
  nav_panel(
    title = "Business Insights",
    
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Key Quantitative Findings (Data-Driven)"),
        card_body(
          uiOutput("business_findings_ui")
        )
      ),
      card(
        card_header("System Optimization & Flexibility Drivers"),
        card_body(
          uiOutput("flexibility_drivers_ui")
        )
      )
    ),
    
    card(
      card_header("Actionable Strategic Recommendations for Indian Power System"),
      card_body(
        div(
          class = "rec-card rec-priority-high",
          tags$h5(class = "mb-1 text-danger", "1. Battery Energy Storage (BESS) & Pumped Hydro Storage (PSP) Deployment"),
          tags$p("Data reveals that solar generation peaks sharply around 12:00-13:00 IST (contributing up to ~24% of instantaneous demand), followed by an evening net ramp of +7 to +38 GW between 17:00 and 20:00 IST.",
                 tags$b("Recommendation: "), "Deploy 4-to-6 hour BESS and expedite Pumped Storage Projects specifically charged during the 11:00-14:00 solar window to discharge during the critical 18:30-22:00 peak.")
        ),
        div(
          class = "rec-card rec-priority-high",
          tags$h5(class = "mb-1 text-danger", "2. Dynamic Time-of-Day (ToD) Tariff Harmonization"),
          tags$p("Industrial and commercial tariffs currently penalize evening consumption but do not sufficiently discount afternoon solar abundance. ",
                 tags$b("Recommendation: "), "State Electricity Regulatory Commissions (SERCs) should mandate steep tariff discounts (25-30%) for industrial consumption and agricultural cold storage during solar hours (10:00-15:00) to incentivize daytime load shifting.")
        ),
        div(
          class = "rec-card rec-priority-med",
          tags$h5(class = "mb-1 text-warning", "3. Wind-Solar Seasonal Hybridization & Grid Interconnections"),
          tags$p("Empirical correlation between solar and wind generation is negative (-0.20), and wind generation surges by over 124% during monsoon months when solar output falls by 23% due to cloud cover.",
                 tags$b("Recommendation: "), "Prioritize hybrid wind-solar park approvals and strengthen high-voltage green energy corridors linking wind-dense western/southern states (Gujarat, Tamil Nadu, Karnataka) to northern and eastern demand load centers.")
        ),
        div(
          class = "rec-card rec-priority-med",
          tags$h5(class = "mb-1 text-warning", "4. Solar Feeder Segregation & PM-KUSUM Scheme Scaling"),
          tags$p("Agricultural pumping represents ~18-20% of Indian electricity demand. ",
                 tags$b("Recommendation: "), "Accelerate solar feeder separation so agricultural pump sets operate exclusively during daytime solar hours, directly eliminating evening agricultural demand spikes.")
        ),
        div(
          class = "rec-card rec-priority-strategic",
          tags$h5(class = "mb-1 text-primary", "5. Thermal Power Plant Cycling & Ramping Flexibility Framework"),
          tags$p("As solar penetration expands toward 2030 targets, the evening ramp requirement will escalate. ",
                 tags$b("Recommendation: "), "Implement Central Electricity Regulatory Commission (CERC) compensation mechanisms for super-critical thermal units providing rapid ramping and minimum turndown (down to 40% technical minimum) during mid-day solar hours.")
        )
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 7: DATA EXPLORER & EXPORT
  # ----------------------------------------------------------------------------
  nav_panel(
    title = "Data Explorer",
    
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        span("Filtered Dataset Records (1-Hour Resolution)"),
        downloadButton("btn_download_csv", "Download Filtered CSV", class = "btn-sm btn-success")
      ),
      card_body(
        DTOutput("table_data_explorer")
      )
    ),
    
    card(
      card_header("Dataset Attribution & Variable Metadata"),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          div(
            tags$h6("Dataset Provenance:"),
            tags$ul(
              tags$li(tags$b("Dataset: "), "Electricity Demand, Solar and Wind Generation Data of India at 1-Hour Interval"),
              tags$li(tags$b("Publisher: "), "Mendeley Data, Elsevier"),
              tags$li(tags$b("Authors: "), "Debanjan Mukherjee, Karuna Kalita, Subhash Kumar (2025)"),
              tags$li(tags$b("DOI: "), tags$a(href = "https://doi.org/10.17632/y58jknpgs8.2", target = "_blank", "10.17632/y58jknpgs8.2")),
              tags$li(tags$b("Primary Source: "), "North-Eastern Regional Load Despatch Centre (NERLDC), Grid Controller of India Limited (Grid-India), Ministry of Power")
            )
          ),
          div(
            tags$h6("Calculations & Definitions:"),
            tags$ul(
              tags$li(tags$b("Renewable Generation: "), "Solar_MW + Wind_MW"),
              tags$li(tags$b("Renewable Penetration (%): "), "(Renewable_MW / Demand_MW) * 100"),
              tags$li(tags$b("Residual Demand (Net Load): "), "Demand_MW - Renewable_MW"),
              tags$li(tags$b("Hourly Ramp Rate: "), "Residual_Demand(t) - Residual_Demand(t-1)"),
              tags$li(tags$b("Units: "), "Power in MW / GW; Energy in MWh / TWh (1 TWh = 1,000,000 MWh)")
            )
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# SERVER LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Load master dataset
  master_data <- load_clean_data()
  
  # Quick Date Presets Observers
  observeEvent(input$btn_all, {
    updateDateRangeInput(session, "date_range", start = min(master_data$Date), end = max(master_data$Date))
  })
  
  observeEvent(input$btn_2024, {
    updateDateRangeInput(session, "date_range", start = as.Date("2024-01-01"), end = as.Date("2024-12-31"))
  })
  
  observeEvent(input$btn_2025, {
    updateDateRangeInput(session, "date_range", start = as.Date("2025-01-01"), end = max(master_data$Date))
  })
  
  observeEvent(input$btn_30d, {
    updateDateRangeInput(session, "date_range", start = max(master_data$Date) - 30, end = max(master_data$Date))
  })
  
  # Reactive Filtered Data
  filtered_df <- reactive({
    req(input$date_range)
    
    df <- master_data %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    
    if (length(input$season_filter) > 0) {
      df <- df %>% filter(Season %in% input$season_filter)
    }
    
    if (input$day_type_filter == "weekday") {
      df <- df %>% filter(!IsWeekend)
    } else if (input$day_type_filter == "weekend") {
      df <- df %>% filter(IsWeekend)
    }
    
    return(df)
  })
  
  # Reactive KPIs
  kpis <- reactive({
    calc_kpis(filtered_df())
  })
  
  # ----------------------------------------------------------------------------
  # TAB 1: OVERVIEW OUTPUTS
  # ----------------------------------------------------------------------------
  output$kpi_total_re <- renderText({
    paste0(kpis()$total_re_twh, " TWh")
  })
  output$kpi_re_share_sub <- renderText({
    paste0(kpis()$re_penetration_pct, "% of gross electricity demand")
  })
  
  output$kpi_solar_twh <- renderText({
    paste0(kpis()$total_solar_twh, " TWh")
  })
  output$kpi_solar_peak_sub <- renderText({
    paste0("Peak generation: ", kpis()$peak_solar_gw, " GW")
  })
  
  output$kpi_wind_twh <- renderText({
    paste0(kpis()$total_wind_twh, " TWh")
  })
  output$kpi_wind_peak_sub <- renderText({
    paste0("Peak generation: ", kpis()$peak_wind_gw, " GW")
  })
  
  output$kpi_demand_twh <- renderText({
    paste0(kpis()$total_demand_twh, " TWh")
  })
  output$kpi_demand_peak_sub <- renderText({
    paste0("Peak demand: ", kpis()$peak_demand_gw, " GW")
  })
  
  output$kpi_avg_demand <- renderText({
    paste0(kpis()$avg_demand_gw, " GW")
  })
  
  output$kpi_max_re_share <- renderText({
    paste0(kpis()$max_re_share_pct, "%")
  })
  
  output$kpi_max_ramp <- renderText({
    paste0("+", kpis()$max_net_ramp_gw, " GW/hr")
  })
  
  output$kpi_crit_shortfall <- renderText({
    paste0(kpis()$critical_re_hrs, " hrs (", kpis()$critical_re_pct, "%)")
  })
  
  output$plot_overview_timeline <- renderPlotly({
    plot_overview_timeline(filtered_df(), agg = input$overview_agg)
  })
  
  output$overview_summary_text <- renderUI({
    df <- filtered_df()
    if (nrow(df) == 0) return("No records match the current filter selection.")
    
    kp <- kpis()
    tags$div(
      tags$p(
        sprintf("Analyzing %s records across %s days (%s to %s).",
                comma(kp$total_records),
                comma(as.numeric(difftime(input$date_range[2], input$date_range[1], units = 'days')) + 1),
                input$date_range[1], input$date_range[2])
      ),
      tags$ul(
        tags$li(sprintf("Clean renewable sources supplied a total of %s TWh, accounting for %s%% of all electricity demanded in India during this period.",
                        kp$total_re_twh, kp$re_penetration_pct)),
        tags$li(sprintf("Solar generation delivered %s TWh (peak of %s GW), while wind delivered %s TWh (peak of %s GW).",
                        kp$total_solar_twh, kp$peak_solar_gw, kp$total_wind_twh, kp$peak_wind_gw)),
        tags$li(sprintf("Peak instantaneous renewable penetration reached %s%%, demonstrating significant clean power integration into the national grid.",
                        kp$max_re_share_pct)),
        tags$li(sprintf("The maximum 1-hour upward ramp on dispatchable generation reached +%s GW/hr, highlighting the critical role of grid flexibility reserves.",
                        kp$max_net_ramp_gw))
      )
    )
  })
  
  # ----------------------------------------------------------------------------
  # TAB 2: GENERATION OUTPUTS
  # ----------------------------------------------------------------------------
  output$plot_diurnal_profile <- renderPlotly({
    plot_diurnal_generation_profile(filtered_df())
  })
  
  output$plot_seasonal_gen <- renderPlotly({
    plot_seasonal_monthly_generation(filtered_df(), view_by = input$seasonal_view_mode)
  })
  
  output$plot_duration <- renderPlotly({
    plot_duration_curve(filtered_df())
  })
  
  output$table_generation_milestones <- renderDT({
    df <- filtered_df()
    if (nrow(df) == 0) return(datatable(data.frame(Message = "No data")))
    
    top_solar <- df %>% slice_max(order_by = Solar_MW, n = 3) %>%
      transmute(Milestone = "Top Solar Hour", Timestamp = as.character(Timestamp),
                Value_GW = round(Solar_MW / 1000, 2), Demand_GW = round(Demand_MW / 1000, 2), RE_Share = paste0(Renewable_Share_Pct, "%"))
    
    top_wind <- df %>% slice_max(order_by = Wind_MW, n = 3) %>%
      transmute(Milestone = "Top Wind Hour", Timestamp = as.character(Timestamp),
                Value_GW = round(Wind_MW / 1000, 2), Demand_GW = round(Demand_MW / 1000, 2), RE_Share = paste0(Renewable_Share_Pct, "%"))
    
    top_re_share <- df %>% slice_max(order_by = Renewable_Share_Pct, n = 3) %>%
      transmute(Milestone = "Top RE Share %", Timestamp = as.character(Timestamp),
                Value_GW = round(Renewable_MW / 1000, 2), Demand_GW = round(Demand_MW / 1000, 2), RE_Share = paste0(Renewable_Share_Pct, "%"))
    
    milestones <- bind_rows(top_solar, top_wind, top_re_share)
    
    datatable(milestones, options = list(pageLength = 9, dom = "t"), rownames = FALSE)
  })
  
  # ----------------------------------------------------------------------------
  # TAB 3: GENERATION VS DEMAND OUTPUTS
  # ----------------------------------------------------------------------------
  output$plot_duck_curve <- renderPlotly({
    plot_duck_curve(filtered_df(), season_filter = input$duck_season)
  })
  
  output$plot_ramp_dist <- renderPlotly({
    plot_ramp_rate_distribution(filtered_df())
  })
  
  output$plot_penetration_dist <- renderPlotly({
    df <- filtered_df()
    if (nrow(df) == 0) return(plotly_empty())
    
    plot_ly(x = df$Renewable_Share_Pct, type = "histogram",
            marker = list(color = "#10B981", line = list(color = "#047857", width = 0.5)),
            nbinsx = 40) %>%
      layout(
        title = list(text = "<b>Distribution of Hourly Renewable Penetration (%)</b>", font = list(size = 14)),
        xaxis = list(title = "Renewable Generation as % of Demand", ticksuffix = "%"),
        yaxis = list(title = "Hours"),
        margin = list(t = 40, b = 40, l = 50, r = 20)
      )
  })
  
  # ----------------------------------------------------------------------------
  # TAB 4: PERFORMANCE & FLEXIBILITY OUTPUTS
  # ----------------------------------------------------------------------------
  output$plot_operational_heatmap <- renderPlotly({
    plot_operational_heatmap(filtered_df(), metric = input$heatmap_metric)
  })
  
  output$plot_correlation_scatter <- renderPlotly({
    df <- filtered_df()
    if (nrow(df) == 0) return(plotly_empty())
    
    # Sample down to max 1,500 points for fluid rendering
    plot_sub <- if (nrow(df) > 1500) slice_sample(df, n = 1500) else df
    
    pair <- input$scatter_pair
    if (pair == "solar_wind") {
      p <- plot_ly(data = plot_sub, x = ~Solar_MW / 1000, y = ~Wind_MW / 1000,
                   type = "scatter", mode = "markers",
                   marker = list(color = "#0284C7", opacity = 0.5, size = 5)) %>%
        layout(title = list(text = "<b>Solar vs Wind Generation (GW)</b>"),
               xaxis = list(title = "Solar Generation (GW)"),
               yaxis = list(title = "Wind Generation (GW)"))
    } else if (pair == "demand_solar") {
      p <- plot_ly(data = plot_sub, x = ~Solar_MW / 1000, y = ~Demand_MW / 1000,
                   type = "scatter", mode = "markers",
                   marker = list(color = "#F59E0B", opacity = 0.5, size = 5)) %>%
        layout(title = list(text = "<b>Demand vs Solar Generation (GW)</b>"),
               xaxis = list(title = "Solar Generation (GW)"),
               yaxis = list(title = "Electricity Demand (GW)"))
    } else if (pair == "demand_wind") {
      p <- plot_ly(data = plot_sub, x = ~Wind_MW / 1000, y = ~Demand_MW / 1000,
                   type = "scatter", mode = "markers",
                   marker = list(color = "#0284C7", opacity = 0.5, size = 5)) %>%
        layout(title = list(text = "<b>Demand vs Wind Generation (GW)</b>"),
               xaxis = list(title = "Wind Generation (GW)"),
               yaxis = list(title = "Electricity Demand (GW)"))
    } else {
      p <- plot_ly(data = plot_sub, x = ~Renewable_MW / 1000, y = ~Demand_MW / 1000,
                   type = "scatter", mode = "markers",
                   marker = list(color = "#10B981", opacity = 0.5, size = 5)) %>%
        layout(title = list(text = "<b>Demand vs Total Renewable Generation (GW)</b>"),
               xaxis = list(title = "Total Renewable (GW)"),
               yaxis = list(title = "Electricity Demand (GW)"))
    }
    
    p %>% layout(margin = list(t = 40, b = 40, l = 50, r = 20))
  })
  
  output$scatter_stats_text <- renderUI({
    df <- filtered_df()
    if (nrow(df) == 0) return(NULL)
    
    pair <- input$scatter_pair
    r_val <- if (pair == "solar_wind") {
      cor(df$Solar_MW, df$Wind_MW, use = "complete.obs")
    } else if (pair == "demand_solar") {
      cor(df$Solar_MW, df$Demand_MW, use = "complete.obs")
    } else if (pair == "demand_wind") {
      cor(df$Wind_MW, df$Demand_MW, use = "complete.obs")
    } else {
      cor(df$Renewable_MW, df$Demand_MW, use = "complete.obs")
    }
    
    div(
      class = "small text-muted mt-2",
      tags$b(sprintf("Pearson Correlation Coefficient (r): %.3f", r_val)),
      p(if (r_val < 0) {
        "Negative correlation indicates natural anti-coincidence, where one source buffers or offsets the other, providing grid stability."
      } else {
        "Positive correlation indicates coincident behavior with grid load requirements."
      })
    )
  })
  
  output$table_performance_metrics <- renderTable({
    df <- filtered_df()
    if (nrow(df) == 0) return(data.frame(Status = "No data"))
    
    data.frame(
      Indicator = c(
        "Solar Variability (Coefficient of Variation)",
        "Wind Variability (Coefficient of Variation)",
        "Demand Variability (Coefficient of Variation)",
        "Solar-to-Wind Correlation",
        "Average Daytime Solar Capacity Proxy (GW)",
        "Average Monsoon Wind Generation (GW)",
        "Max 1-Hour Evening Ramp Requirement (GW/hr)",
        "Hours with RE Share < 5% (Baseload Critical)"
      ),
      Value = c(
        sprintf("%.2f%%", (sd(df$Solar_MW[df$Hour >= 6 & df$Hour <= 18], na.rm = TRUE) / mean(df$Solar_MW[df$Hour >= 6 & df$Hour <= 18], na.rm = TRUE)) * 100),
        sprintf("%.2f%%", (sd(df$Wind_MW, na.rm = TRUE) / mean(df$Wind_MW, na.rm = TRUE)) * 100),
        sprintf("%.2f%%", (sd(df$Demand_MW, na.rm = TRUE) / mean(df$Demand_MW, na.rm = TRUE)) * 100),
        sprintf("%.3f", cor(df$Solar_MW, df$Wind_MW, use = "complete.obs")),
        sprintf("%.2f GW", mean(df$Solar_MW[df$Hour %in% 10:14], na.rm = TRUE) / 1000),
        sprintf("%.2f GW", mean(df$Wind_MW[df$Season == "Monsoon"], na.rm = TRUE) / 1000),
        sprintf("+%.2f GW/hr", max(df$Net_Ramp_MW, na.rm = TRUE) / 1000),
        sprintf("%d hours (%.1f%% of period)", sum(df$Renewable_Share_Pct < 5), (sum(df$Renewable_Share_Pct < 5) / nrow(df)) * 100)
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ----------------------------------------------------------------------------
  # TAB 5: FORECASTING OUTPUTS
  # ----------------------------------------------------------------------------
  forecast_reactive <- eventReactive(input$btn_run_forecast, {
    run_time_series_forecast(
      df         = master_data, # Use full time series for proper seasonality
      target     = input$forecast_target,
      horizon    = input$forecast_horizon,
      test_split = input$forecast_holdout
    )
  }, ignoreNULL = FALSE)
  
  output$fc_mape <- renderText({
    paste0(forecast_reactive()$metrics$MAPE, "%")
  })
  output$fc_rmse <- renderText({
    paste0(forecast_reactive()$metrics$RMSE, " GW")
  })
  output$fc_mae <- renderText({
    paste0(forecast_reactive()$metrics$MAE, " GW")
  })
  output$fc_coverage <- renderText({
    paste0(forecast_reactive()$metrics$Coverage_95, "%")
  })
  
  output$plot_forecast <- renderPlotly({
    plot_forecast_results(forecast_reactive())
  })
  
  output$table_future_forecast <- renderDT({
    fc <- forecast_reactive()$future_df
    datatable(fc, options = list(pageLength = 7, dom = "tip"), rownames = FALSE) %>%
      formatRound(columns = c("Forecast", "Lower_95", "Upper_95"), digits = 2)
  })
  
  # ----------------------------------------------------------------------------
  # TAB 6: BUSINESS INSIGHTS OUTPUTS
  # ----------------------------------------------------------------------------
  insights_data <- reactive({
    generate_business_insights(filtered_df())
  })
  
  output$business_findings_ui <- renderUI({
    ins <- insights_data()
    if (length(ins) == 0) return(p("No data available."))
    
    tags$div(
      tags$div(
        class = "mb-3",
        tags$b("☀️ Peak Solar Window: "),
        sprintf("Solar generation peaks consistently at Hour %d:00 IST, averaging %s GW across the selected timeframe and delivering up to %s%% of total instant national grid load.",
                ins$peak_solar_hour, ins$peak_solar_mean_gw, ins$peak_solar_share_pct)
      ),
      tags$div(
        class = "mb-3",
        tags$b("📉 Evening Ramp Crunch (17:00 to 20:00 IST): "),
        sprintf("As solar drops to zero during sunset while residential lighting and cooling demand surges, the residual net load ramps up by an average of +%s GW (reaching a peak ramp of +%s GW in 3 hours).",
                ins$avg_evening_ramp_gw, ins$max_evening_ramp_gw)
      ),
      tags$div(
        class = "mb-3",
        tags$b("🌪️ Monsoon Wind Surge & Cloud Cover Penalty: "),
        sprintf("During the Indian monsoon (June–September), wind generation increases by +%s%% compared to winter levels, actively compensating for the %s%% solar generation decline caused by heavy cloud cover.",
                ins$wind_monsoon_boost_pct, ins$solar_monsoon_drop_pct)
      ),
      tags$div(
        class = "mb-3",
        tags$b("⚠️ Critical RE Shortfall Periods: "),
        sprintf("A total of %s hours (%s%% of time) experience renewable generation contributing under 5%% of grid demand (predominantly night hours and calm winter mornings), requiring full conventional backup.",
                comma(ins$crit_shortfall_hours), ins$crit_shortfall_pct)
      )
    )
  })
  
  output$flexibility_drivers_ui <- renderUI({
    ins <- insights_data()
    if (length(ins) == 0) return(p("No data available."))
    
    tags$div(
      tags$div(
        class = "mb-3",
        tags$b("🔄 Natural Wind-Solar Anti-Coincidence: "),
        sprintf("The Pearson correlation between solar and wind is r = %.3f. The negative correlation provides a natural smoothing benefit to the national grid network.",
                ins$solar_wind_correlation)
      ),
      tags$div(
        class = "mb-3",
        tags$b("🔋 Energy Storage System Sizing Window: "),
        "Storage assets must be scheduled to absorb up to 35-50 GW of surplus solar energy between 10:00 and 14:00 IST, and discharge during the 18:30 to 22:30 evening peak to prevent thermal generator stress."
      ),
      tags$div(
        class = "mb-3",
        tags$b("🏭 Demand Response Potential: "),
        "Agricultural solar feeders and industrial cold chains represent over 30 GW of shiftable load that can be mandated to operate exclusively during daytime solar hours."
      )
    )
  })
  
  # ----------------------------------------------------------------------------
  # TAB 7: DATA EXPLORER & EXPORT OUTPUTS
  # ----------------------------------------------------------------------------
  output$table_data_explorer <- renderDT({
    df <- filtered_df() %>%
      transmute(
        Timestamp = as.character(Timestamp),
        Demand_GW = round(Demand_MW / 1000, 2),
        Solar_GW  = round(Solar_MW / 1000, 2),
        Wind_GW   = round(Wind_MW / 1000, 2),
        Renewable_GW = round(Renewable_MW / 1000, 2),
        RE_Share = paste0(Renewable_Share_Pct, "%"),
        Net_Demand_GW = round(Residual_Demand_MW / 1000, 2),
        Net_Ramp_GW = round(Net_Ramp_MW / 1000, 2),
        Season = as.character(Season),
        Tier = as.character(RE_Tier)
      )
    
    datatable(
      df,
      filter = "top",
      options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  output$btn_download_csv <- downloadHandler(
    filename = function() {
      paste0("india_renewable_energy_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_df(), file, row.names = FALSE)
    }
  )
}

# ------------------------------------------------------------------------------
# LAUNCH APPLICATION
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
