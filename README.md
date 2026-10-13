# Renewable Energy Intelligence: Solar Generation, Electricity Demand and Energy Performance Analytics Using R

An interactive Business Intelligence (BI) dashboard built with **R**, **Shiny**, and **bslib (Bootstrap 5)** analyzing 1-hour interval electricity demand, solar generation, wind generation, and grid performance metrics across India.

---

## 1. Project Overview & Highlights

- **Real-World Empirical Dataset:** Ingests official PMU SCADA data provided by the North-Eastern Regional Load Despatch Centre (NERLDC), Grid Controller of India Limited (Grid-India), Ministry of Power, Government of India.
- **Coverage:** 12,984 consecutive hourly observations spanning January 1, 2024 to June 24, 2025 across all Indian regional grid networks.
- **Interactive Analytics:**
  - **Overview:** Executive KPIs (TWh generation, demand, penetration %, peak capacity, and ramp rates).
  - **Generation Analysis:** 24-hour diurnal profiles with 10th-90th percentile ribbons, seasonal comparisons, and load duration curves.
  - **Generation vs. Demand:** Detailed examination of the Indian Grid **Duck Curve**, net load residual demand, and 1-hour ramp distributions.
  - **Energy Performance:** Diurnal-seasonal 2D operational heatmaps, correlation scatter plots, and system flexibility indicators.
  - **Time-Series Forecasting:** Holt-Winters exponential smoothing with weekly seasonality, evaluated on an out-of-sample chronological holdout set (MAPE ~6.16%, 96.3% coverage of 95% prediction interval).
  - **Business Insights & Recommendations:** Data-driven operational findings covering peak solar windows, evening ramping crunch, monsoon wind surge (+124.6%), and strategic storage/tariff recommendations.
  - **Data Explorer & Export:** Dynamic searchable DataTable with CSV export functionality.

---

## 2. Project Structure

```
renewable-energy-intelligence/
├── app.R                                  # Main Shiny dashboard application
├── R/
│   ├── data_processing.R                  # Data ingestion, telemetry cleaning & feature engineering
│   └── analysis.R                         # Analytics, KPI metrics, plotting & forecasting engine
├── data/
│   ├── raw/
│   │   ├── January 2024- June 2025.xlsx   # Official raw Excel dataset from Mendeley Data
│   │   └── india_electricity_re_2024_2025_raw.csv # Raw CSV data export
│   └── processed/
│       ├── india_electricity_re_clean.csv # Cleaned CSV with engineered features
│       └── india_electricity_re_clean.rds # Fast-loading binary RDS cache
├── renewable-energy-intelligence.Rproj    # RStudio project configuration
├── .gitignore
└── README.md
```

---

## 3. Technology Stack & Prerequisites

- **Language:** R (>= 4.2.0, tested on R 4.6.1)
- **Core Packages:**
  - `shiny` (>= 1.8.0)
  - `bslib` (>= 0.6.0)
  - `dplyr`, `tidyr`, `lubridate`
  - `ggplot2`, `plotly`
  - `DT`, `scales`, `bsicons`

### Package Installation in R

Run the following command inside R or RStudio:

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "tidyr", 
  "lubridate", "ggplot2", "plotly", "DT", 
  "scales", "bsicons"
))
```

---

## 4. Running the Application

### Option A: From R or RStudio
Open `renewable-energy-intelligence.Rproj` or set your working directory to the project root, then execute:

```r
shiny::runApp()
```

### Option B: From Command Line / Terminal
Run:

```bash
Rscript -e "shiny::runApp(port = 8888, launch.browser = TRUE)"
```

Then open your browser at `http://127.0.0.1:8888`.

---

## 5. Dataset Attribution & Reference

- **Title:** Electricity Demand, Solar and Wind Generation Data of India at 1-hour interval
- **Authors:** Debanjan Mukherjee, Karuna Kalita, Subhash Kumar (2025)
- **Repository:** Mendeley Data, Elsevier
- **DOI:** [10.17632/y58jknpgs8.2](https://doi.org/10.17632/y58jknpgs8.2)
- **Primary Source:** North-Eastern Regional Load Despatch Centre (NERLDC), Grid Controller of India Limited (Grid-India), Ministry of Power, Government of India.
