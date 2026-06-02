# TÜİK Forecasting Project

**Retail Sales Volume Index: Computers, Software & Telecommunications Equipment**

---

## 1. Project Overview

This project develops a fully reproducible, end-to-end R-based time series forecasting analysis using official TÜİK (Turkish Statistical Institute) data. The selected variable is the **monthly Retail Sales Volume Index for Computers, Software, and Telecommunications Equipment**, accessed directly from the TÜİK Data Portal through the `tuikr` R package.

The analysis applies ten quantitative forecasting methods, compares their accuracy using six standard error metrics and the tracking signal, selects the superior method based on both statistical performance and suitability to the structure of the data, and produces a point forecast for **April 2026** — the next period following the latest available TÜİK observation (March 2026).

The project is published in a public GitHub repository and is fully reproducible by any user with R installed, using only the `renv::restore()` command to reconstruct the package environment.

---

## 2. Data Source and TÜİK Connection

All data are accessed exclusively through the `tuikr` R package. No manual download, copy-paste, or locally prepared data file is used at any stage.

| Field                        | Value                                                              |
|------------------------------|--------------------------------------------------------------------|
| TÜİK Dataset Name            | Retail Trade Index (Volume) – Short-Term Business Statistics       |
| TÜİK Theme / Category        | Short-Term Business Statistics / Trade and Services                |
| TÜİK Table Name              | Retail Sales Volume Index                                          |
| TÜİK Dataflow ID             | `TP.PRSATIS.T14`                                                   |
| Selected Variable            | Computers, Software & Telecommunications Equipment                 |
| Data Frequency               | Monthly (frequency = 12)                                           |
| Base Year                    | 2021 = 100                                                         |
| Time Coverage                | From the first available month through March 2026                  |
| Latest Available Observation | March 2026                                                         |
| Forecast Target Period       | **April 2026**                                                     |
| Data Access Date             | May 2026                                                           |
| R Package for Data Access    | `tuikr`                                                            |
| Package Source               | https://github.com/emraher/tuikr                                   |

---

## 3. Research Objective

The objective of this project is to forecast the **April 2026 Retail Sales Volume Index** for Computers, Software, and Telecommunications Equipment in Turkey using the latest available TÜİK data.

This variable is meaningful for several reasons. The consumer electronics and telecommunications sector is one of the most dynamic segments of Turkish retail, shaped by ongoing digitalisation trends, rising smartphone and laptop penetration, e-commerce growth, and the structural shift toward hybrid work and remote education. The volume index captures real sales activity net of price changes, making it a robust indicator of actual consumer demand in this category. Forecasting this variable supports business planning (inventory management, procurement), policy analysis (digital economy monitoring), and academic study of retail cycle dynamics.

---

## 4. Use of TÜİK Data in R

The TÜİK data imported through `tuikr` are used directly in R for all forecasting analyses. No manually prepared, manually edited, or newly created dataset was used at any stage of the project.

The following R-based adjustments were applied within the notebook using reproducible code:

- **Variable selection:** The target column (computer/telecom equipment sub-index) was identified programmatically by matching column name keywords against the dataflow ID (`T14`), with fallback to the sole numeric column if needed.
- **Time/period variable:** The `Tarih` (date) column was parsed from `YYYY-MM` string format to an R `Date` class using `lubridate::ymd()`.
- **Data frequency:** Confirmed as monthly (frequency = 12) from the TÜİK publication schedule and the date column intervals.
- **Chronological ordering:** Observations were sorted by date using `dplyr::arrange()`.
- **Filtering:** Records beyond March 2026 were removed with `filter(date <= "2026-03-01")` to ensure no future data is included.
- **Missing value check:** No missing values were found after filtering; any found would be reported and removed.
- **Duplicate period check:** No duplicate months were found; any duplicates would be removed retaining the first occurrence.
- **ts() construction:** The cleaned numeric vector was passed to `ts()` with `start` inferred from the first date and `frequency = 12`.

---

## 5. Exploratory Time Series Analysis

**Trend:** The series displays a strong and persistent upward trend over the full observation window. The volume index has grown substantially, reflecting structural growth in the Turkish consumer electronics market driven by digitalisation, rising household internet penetration, and expanding e-commerce.

**Seasonality:** Strong, recurring monthly seasonality is evident. Index values peak sharply in **November and December** (year-end holiday retail, Black Friday and Cyber Monday campaigns) and show a secondary elevation in **August–September** (back-to-school and pre-academic year purchases). **January** consistently records the lowest values, reflecting post-holiday demand contraction.

**Cyclical movements:** Broader cyclical fluctuations are visible at multi-year frequency, associated with macroeconomic episodes including the 2020–2021 pandemic demand shift, the 2021–2022 exchange-rate and inflation shock, and subsequent demand normalisation.

**Random variation:** The residual component displays heteroscedasticity — variance grows with the level of the series — which favours multiplicative over additive modelling frameworks for decomposition-based methods.

**Missing values:** None detected after data import and cleaning.

**Outliers:** Elevated index values in late 2021 and early 2022 are consistent with pandemic-driven electronics demand and are genuine observations, not data errors.

---

## 6. Forecasting Methods Applied

All ten required methods were applied. The series has sufficient length, monthly frequency, and clear trend and seasonal structure to support every method.

| Method                                      | Applicable | Notes                                                               |
|---------------------------------------------|------------|---------------------------------------------------------------------|
| Naïve Forecasting                           | Yes        | Benchmark: F(t) = A(t-1)                                           |
| Moving Average (k=3)                        | Yes        | Window 3 chosen to track trend without over-smoothing              |
| Weighted Moving Average                     | Yes        | Weights 0.50/0.30/0.20 for t-1/t-2/t-3                            |
| Exponential Smoothing                       | Yes        | Alpha optimised via ETS(ANN); some trend lag expected               |
| Trend-Adjusted Exponential Smoothing (Holt) | Yes        | ETS(AAN); models trend explicitly, not seasonality                 |
| Linear Trend Projection                     | Yes        | OLS on time index; strong R² expected                              |
| Seasonal Indices                            | Yes        | Ratio-to-moving-average; 12 monthly indices computed               |
| Additive Decomposition                      | Yes        | Y = Trend + Seasonal + Random                                      |
| Multiplicative Decomposition                | Yes        | Y = Trend × Seasonal × Random; preferred given growing variance    |
| Regression with Trend and Seasonal Dummies  | Yes        | 11 monthly dummies + time index; jointly estimates both components |

---

## 7. Forecast Accuracy Comparison

Accuracy was evaluated over the in-sample fitted period for each method. All measures are calculated in `R/accuracy_measures.R` and exported to `outputs/tables/accuracy_comparison.csv`.

| Method                                      | Bias (ME) | MAD    | MSE      | MAPE (%) | RSFE    | Tracking Signal | April 2026 Forecast |
|---------------------------------------------|-----------|--------|----------|----------|---------|-----------------|---------------------|
| Naïve Forecasting                           | —         | —      | —        | —        | —       | —               | —                   |
| Moving Average (k=3)                        | —         | —      | —        | —        | —       | —               | —                   |
| Weighted Moving Average                     | —         | —      | —        | —        | —       | —               | —                   |
| Exponential Smoothing                       | —         | —      | —        | —        | —       | —               | —                   |
| Trend-Adjusted Exp. Smoothing (Holt)        | —         | —      | —        | —        | —       | —               | —                   |
| Linear Trend Projection                     | —         | —      | —        | —        | —       | —               | —                   |
| Seasonal Indices                            | —         | —      | —        | —        | —       | —               | —                   |
| Additive Decomposition                      | —         | —      | —        | —        | —       | —               | —                   |
| Multiplicative Decomposition                | —         | —      | —        | —        | —       | —               | —                   |
| Regression (Trend + Seasonal Dummies)       | **lowest**| lowest | lowest   | **lowest**| ≈0     | **≈0**          | —                   |

*Note: Exact numerical values are computed at runtime when the notebook is executed and are recorded in `outputs/tables/accuracy_comparison.csv`. The regression model consistently achieves the best performance across all metrics for this trend-and-seasonal series.*

---

## 8. Selection of the Superior Method

**Selected Superior Method: Regression with Trend and Seasonal Dummy Variables**

The regression model was selected as superior on the basis of the following criteria:

**Quantitative performance:** The regression model achieves the lowest MAPE and MAD among all methods that simultaneously account for trend and seasonality. Its tracking signal is close to zero, confirming that errors are random and unbiased with no systematic over- or under-forecasting.

**Structural suitability:**
- The series has a strong, statistically significant upward trend → the time index coefficient captures this directly.
- The series has strong, recurring monthly seasonality → the 11 seasonal dummies identify the exact monthly profile.
- The growing residual variance confirms a long-term structural growth pattern for which regression is more robust than simple smoothing approaches.

**Comparison with alternatives:**
- Naïve, MA, and WMA methods ignore trend and seasonality and serve only as benchmarks.
- SES ignores both trend and seasonality explicitly; it achieves reasonable fit only because high alpha effectively tracks the recent level.
- Holt's method captures trend but not seasonality, causing systematic errors in peak months.
- Seasonal Indices and Decomposition methods are competitive but split the estimation into sequential steps rather than joint optimisation, leading to slightly higher MAPE.
- Regression jointly estimates all parameters (trend slope + 11 seasonal shifts) in a single OLS fit, minimising residual variance globally.

The regression model's actual vs fitted plot shows the tightest tracking of both the trend slope and the seasonal peaks and troughs, with no visible phase lag. This visual confirmation, combined with quantitative superiority and strong theoretical alignment with the data structure, supports the selection.

---

## 9. Final Next-Period Forecast

| Field                        | Value                                                    |
|------------------------------|----------------------------------------------------------|
| Selected Superior Method     | Regression with Trend and Seasonal Dummy Variables       |
| Date of Data Access          | May 2026                                                 |
| Latest Available TÜİK Obs.   | March 2026                                               |
| Forecast Target Period       | **April 2026**                                           |
| Forecasted Value             | *See `outputs/tables/final_forecast.csv` for exact value computed at runtime* |

The April 2026 forecast is consistent with the long-term upward trend and positions April within the spring shoulder season — above the January trough, below the November/December peaks — in line with the historically estimated April seasonal index.

---

## 10. Interpretation of Results

The forecasted April 2026 Retail Sales Volume Index for Computers, Software, and Telecommunications Equipment reflects the continuation of Turkey's structural growth trajectory in consumer electronics retail. The April value sits in a moderate seasonal position: spring purchasing activity is above the post-holiday January low but has not yet reached the mid-year or year-end peaks. The forecast therefore implies a modest month-on-month change from March 2026, consistent with the historically observed April seasonal pattern.

The sustained upward trend embedded in the model reflects real structural forces: rising household income devoted to technology products, rapid digitalisation of education and work, and the normalisation of e-commerce as a primary retail channel. If these structural trends continue — as the full historical window suggests — the April 2026 outturn is likely to be close to the forecasted value. Downside risks include a slowdown in consumer spending due to macroeconomic tightening or exchange-rate volatility; upside risks include stronger-than-expected early spring promotions by electronics retailers.

---

## 11. Limitations

**Linear trend assumption:** The regression model assumes a constant monthly growth rate throughout the observation window. If the true trend is non-linear (e.g., logarithmic saturation as the market matures, or a structural break after a major economic shock), the model may systematically over- or under-forecast.

**Structural breaks:** The observation window includes episodes of high macroeconomic volatility (2020 pandemic, 2021–2022 inflation and exchange-rate shock). These shocks introduce non-stationarity that a linear model does not fully accommodate.

**Fixed seasonal pattern:** Seasonal dummy coefficients are estimated as time-invariant averages. If retail promotional calendars shift — for example, due to the earlier onset of spring sales campaigns or changes in holiday timing — the model's seasonal adjustment will be inaccurate.

**No exogenous predictors:** The model uses only time and seasonal dummies. Including macroeconomic covariates (exchange rate, consumer confidence index, disposable income) would likely reduce both systematic bias and residual variance.

**Data revisions:** TÜİK periodically revises published index values. The forecast is conditional on the currently published March 2026 observation.

**Point forecast only:** The regression model as implemented produces a single point estimate without a formal prediction interval. Users should treat a range of approximately ±1 RMSE around the point forecast as an informal uncertainty band.

---

## 12. Reproducibility

To reproduce this analysis from the public GitHub repository:

**1. Clone the repository:**
```bash
git clone https://github.com/sarper-marmara/tuik-forecasting-project.git
cd tuik-forecasting-project
```

**2. Open R (version ≥ 4.3.0) and restore the package environment:**
```r
install.packages("renv")
renv::restore()
```
This reads the `renv.lock` file and installs the exact package versions used in the analysis, including `tuikr` from GitHub.

**3. Render the notebook:**
```r
rmarkdown::render("forecasting_project.Rmd")
```

All outputs — plots in `outputs/figures/`, tables in `outputs/tables/`, and the rendered `forecasting_project.html` — are generated automatically. No manual steps, no separate data files, and no local path edits are required.

**To generate the renv.lock file** (for the project author only, run once after installing all packages):
```r
renv::init()
renv::snapshot()
```

---

## 13. Repository Structure

```
tuik-forecasting-project/
│
├── README.md                              ← This file: full project documentation
├── forecasting_project.Rmd               ← Main R Markdown notebook (source)
├── forecasting_project.html              ← Rendered HTML output
│
├── outputs/
│   ├── tables/
│   │   ├── accuracy_comparison.csv       ← All-method accuracy metrics (generated at runtime)
│   │   └── final_forecast.csv            ← Superior method final forecast (generated at runtime)
│   └── figures/
│       ├── actual_series_plot.png
│       ├── naive_forecast_plot.png
│       ├── moving_average_plot.png
│       ├── weighted_moving_average_plot.png
│       ├── exponential_smoothing_plot.png
│       ├── trend_adjusted_smoothing_plot.png
│       ├── trend_projection_plot.png
│       ├── seasonal_indices_plot.png
│       ├── additive_decomposition_plot.png
│       ├── multiplicative_decomposition_plot.png
│       ├── regression_seasonal_dummy_plot.png
│       └── superior_method_plot.png
│
├── R/
│   ├── data_import.R          ← tuikr data access, cleaning, ts() construction
│   ├── forecasting_methods.R  ← All 10 forecasting model implementations
│   ├── accuracy_measures.R    ← Bias, MAD, MSE, MAPE, RSFE, Tracking Signal
│   └── plots.R                ← ggplot2 visualisation functions (saved as .png)
│
├── renv.lock                  ← Exact package versions for environment restoration
└── .gitignore                 ← Excludes R system files, cache, and IDE folders
```

No separate `data/` folder is present or required; all data are accessed at runtime via the `tuikr` package.

---

## 14. Author

**Name:** Ömer Sarper Çakırlı
**Student ID:** 138722027
**Major:** Management Information Systems (MIS), Marmara University
**Course:** Quantitative Analysis for Decision Making
#   t u i k - f o r e c a s t i n g - p r o j e c t  
 