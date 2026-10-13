<<<<<<< HEAD
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
=======
# Renewable-Energy-Intelligence-Solar-Generation-and-Demand-Analytics-Using-R
# Renewable Energy Intelligence: Solar Generation and Demand Analytics Using R

## Project Overview

Renewable Energy Intelligence is a Business Intelligence project developed using R to explore renewable energy generation and electricity demand through data analysis and visualization.

The project focuses on identifying trends and patterns in energy data, comparing generation and demand where compatible data is available, and presenting analytical findings in a form that supports better understanding and energy-related decision-making.

The application is designed to make energy data easier to explore through interactive visualizations and analytical summaries.

## Objectives

- Analyze available renewable energy generation and electricity demand data.
- Explore temporal patterns and changes in energy generation and demand.
- Visualize important trends through charts and interactive analytical views.
- Compare energy generation and demand when the dataset supports a valid comparison.
- Present data-driven observations that can support energy planning and decision-making.
- Demonstrate the practical use of R programming for Business Intelligence.

## Technology Stack

The technologies used should be confirmed against the final project files.

- **Programming language:** R
- **Application framework:** Shiny, if used in the application
- **Data manipulation:** dplyr and tidyr, if used
- **Date and time processing:** lubridate, if used
- **Data visualization:** ggplot2 and/or plotly, if used
- **Interactive tables:** DT, if used

## Project Structure

The exact structure depends on the files generated during development. A typical structure for this project may look like:

```text
renewable-energy-intelligence/
├── app.R
├── R/
│   ├── data_processing.R
│   └── analysis.R
├── data/
│   ├── raw/
│   └── processed/
├── README.md
└── .gitignore
```

The application may use a different structure. Refer to the actual files in the project folder before running it.

## Dataset

The project uses energy-related data containing the fields required for its implemented analysis.

Before interpreting the results, verify the dataset's source, geographic coverage, time period, units, and available variables. The dataset citation and any applicable usage conditions should be documented here.

**Dataset source:** Add the actual dataset name and source URL used in the final application.

## Methodology

The general analytical workflow consists of:

1. **Data loading:** Import the source data into R.
2. **Data preparation:** Inspect the data, handle missing or inconsistent values where necessary, and prepare variables for analysis.
3. **Exploratory analysis:** Examine distributions, trends, and patterns in the available energy data.
4. **Visualization:** Represent the analysis through suitable charts, summaries, and interactive views.
5. **Interpretation:** Identify findings supported by the dataset and explain their relevance to energy planning.
6. **Presentation:** Communicate the results through the application interface.

Only the steps implemented in the actual source code should be described as completed project functionality.

## How to Run the Project

### Prerequisites

- R installed on your computer.
- RStudio Desktop or another compatible R development environment.
- The project source files and required dataset.

Download R from the [official R Project website](https://www.r-project.org/).

### Run the application

1. Download or copy the complete project folder to your computer.
2. Open the project in RStudio, if an `.Rproj` file is available.
3. Open the main application file, usually `app.R`.
4. Install the packages required by the source code.
5. Run the application using RStudio's **Run App** button or execute the appropriate command in the R console.

If the project uses Shiny and its main file is `app.R`, you can generally start it from the project directory using:
>>>>>>> 42634223fb5e3aa482b2ee9b3bfbd71f5b92360b

```r
shiny::runApp()
```

<<<<<<< HEAD
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
=======
Alternatively, specify the project directory:

```r
shiny::runApp("path/to/renewable-energy-intelligence")
```

The application should open in your default web browser or in an RStudio viewer.

## Expected Output

When the application runs successfully, it should display the interface and analytical features implemented in the project. Depending on the final implementation, these may include energy trend visualizations, demand and generation comparisons, summary statistics, filters, or interactive tables.

The actual outputs depend on the dataset and the features present in the source code.

## Limitations

- Findings depend on the quality, completeness, and coverage of the available data.
- Comparisons are meaningful only when the variables use compatible units, time intervals, and geographic coverage.
- Observed trends should not be interpreted as proof of causation.
- Forecasting, optimization, and energy-efficiency estimates should only be claimed if they have been explicitly implemented and validated.

## Future Scope

Potential extensions include incorporating additional renewable energy sources, adding more granular temporal analysis, implementing validated forecasting methods, and publishing the application online for wider access.

These are possible future enhancements and are not necessarily part of the current implementation.


## Acknowledgement

This project demonstrates the application of R programming and Business Intelligence concepts to renewable energy data analysis and visualization.

## License

No specific license is declared. Add an appropriate license if the project is intended for public distribution.
>>>>>>> 42634223fb5e3aa482b2ee9b3bfbd71f5b92360b
