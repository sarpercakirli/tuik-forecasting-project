# =============================================================================
# R/plots.R
# Purpose: Reusable ggplot2 plot functions for all forecasting methods.
#          Each function saves the plot as a .png to outputs/figures/
# Author : Sarper | MIS, Marmara University
# Course : Quantitative Analysis for Decision Making
# =============================================================================

library(ggplot2)
library(dplyr)

FIGURES_DIR <- "outputs/figures"

# =============================================================================
# HELPER: Build a data frame with time index, actual, and fitted columns
# =============================================================================
build_plot_df <- function(ts_data, fitted_vals, next_forecast = NULL,
                          next_label = "April 2026") {
  n       <- length(ts_data)
  actual  <- as.numeric(ts_data)
  time_df <- data.frame(
    t      = time(ts_data),
    actual = actual,
    fitted = fitted_vals[seq_len(n)]
  )
  # Optionally append the next-period forecast point
  if (!is.null(next_forecast)) {
    next_t <- max(time_df$t) + 1 / frequency(ts_data)
    next_row <- data.frame(t = next_t, actual = NA, fitted = next_forecast)
    time_df  <- bind_rows(time_df, next_row)
  }
  return(time_df)
}

# =============================================================================
# THEME: consistent look across all plots
# =============================================================================
ts_theme <- function() {
  theme_bw(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "grey40"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      axis.title       = element_text(size = 11),
      panel.grid.minor = element_blank()
    )
}

# =============================================================================
# save_plot() ??? wrapper to save a ggplot object as .png
# =============================================================================
save_plot <- function(p, filename, width = 10, height = 5) {
  dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)
  full_path <- file.path(FIGURES_DIR, filename)
  ggsave(full_path, plot = p, width = width, height = height, dpi = 150)
  cat("Plot saved:", full_path, "\n")
  invisible(p)
}

# =============================================================================
# 0. ACTUAL TIME SERIES PLOT
# =============================================================================
plot_actual_series <- function(ts_data) {
  df <- data.frame(t = as.numeric(time(ts_data)), value = as.numeric(ts_data))
  p  <- ggplot(df, aes(x = t, y = value)) +
    geom_line(color = "#2C7BB6", linewidth = 0.9) +
    geom_point(size = 1.2, color = "#2C7BB6", alpha = 0.6) +
    labs(
      title    = "Retail Sales Volume Index: Computers, Software & Telecom Equipment",
      subtitle = "T????K Monthly Data | Source: tuikr | Variable: TP.PRSATIS.T14",
      x        = "Year",
      y        = "Volume Index (2021=100)"
    ) +
    ts_theme()
  save_plot(p, "actual_series_plot.png")
  p
}

# =============================================================================
# 1. NA??VE FORECAST PLOT
# =============================================================================
plot_naive <- function(ts_data, result) {
  df <- build_plot_df(ts_data, result$fitted, result$next_forecast)
  p  <- ggplot(df, aes(x = t)) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 0.9, na.rm = TRUE) +
    geom_line(aes(y = fitted, color = "Na??ve Forecast"), linewidth = 0.8,
              linetype = "dashed", na.rm = TRUE) +
    geom_point(data = tail(df, 1),
               aes(y = fitted, color = "April 2026 Forecast"),
               size = 4, shape = 18) +
    scale_color_manual(values = c("Actual" = "#2C7BB6",
                                  "Na??ve Forecast" = "#D7191C",
                                  "April 2026 Forecast" = "#1A9641")) +
    labs(title    = "Actual vs Na??ve Forecast",
         subtitle = paste0("April 2026 Forecast: ",
                           round(result$next_forecast, 2)),
         x = "Year", y = "Volume Index (2021=100)") +
    ts_theme()
  save_plot(p, "naive_forecast_plot.png")
  p
}

# =============================================================================
# 2. MOVING AVERAGE PLOT
# =============================================================================
plot_ma <- function(ts_data, result) {
  df <- build_plot_df(ts_data, result$fitted, result$next_forecast)
  p  <- ggplot(df, aes(x = t)) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 0.9, na.rm = TRUE) +
    geom_line(aes(y = fitted, color = paste0("MA(", result$window, ")")),
              linewidth = 0.8, linetype = "dashed", na.rm = TRUE) +
    geom_point(data = tail(df, 1),
               aes(y = fitted, color = "April 2026 Forecast"),
               size = 4, shape = 18) +
    scale_color_manual(values = c("Actual" = "#2C7BB6",
                                  "MA(3)"  = "#D7191C",
                                  "April 2026 Forecast" = "#1A9641")) +
    labs(title    = paste0("Actual vs Moving Average (k=", result$window, ")"),
         subtitle = paste0("April 2026 Forecast: ", round(result$next_forecast, 2)),
         x = "Year", y = "Volume Index (2021=100)") +
    ts_theme()
  save_plot(p, "moving_average_plot.png")
  p
}

# =============================================================================
# 3. WEIGHTED MOVING AVERAGE PLOT
# =============================================================================
plot_wma <- function(ts_data, result) {
  df <- build_plot_df(ts_data, result$fitted, result$next_forecast)
  p  <- ggplot(df, aes(x = t)) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 0.9, na.rm = TRUE) +
    geom_line(aes(y = fitted, color = "WMA Forecast"), linewidth = 0.8,
              linetype = "dashed", na.rm = TRUE) +
    geom_point(data = tail(df, 1),
               aes(y = fitted, color = "April 2026 Forecast"),
               size = 4, shape = 18) +
    scale_color_manual(values = c("Actual" = "#2C7BB6",
                                  "WMA Forecast" = "#D7191C",
                                  "April 2026 Forecast" = "#1A9641")) +
    labs(title    = "Actual vs Weighted Moving Average",
         subtitle = paste0("Weights: 0.50 (t-1), 0.30 (t-2), 0.20 (t-3) | ",
                           "April 2026 Forecast: ", round(result$next_forecast, 2)),
         x = "Year", y = "Volume Index (2021=100)") +
    ts_theme()
  save_plot(p, "weighted_moving_average_plot.png")
  p
}

# =============================================================================
# 4. EXPONENTIAL SMOOTHING PLOT
# =============================================================================
plot_ses <- function(ts_data, result) {
  df <- build_plot_df(ts_data, result$fitted, result$next_forecast)
  p  <- ggplot(df, aes(x = t)) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 0.9, na.rm = TRUE) +
    geom_line(aes(y = fitted, color = paste0("SES (??=", result$alpha, ")")),
              linewidth = 0.8, linetype = "dashed", na.rm = TRUE) +
    geom_point(data = tail(df, 1),
               aes(y = fitted, color = "April 2026 Forecast"),
               size = 4, shape = 18) +
    scale_color_manual(values = setNames(
      c("#2C7BB6", "#D7191C", "#1A9641"),
      c("Actual", paste0("SES (??=", result$alpha, ")"), "April 2026 Forecast")
    )) +
    labs(title    = paste0("Actual vs Exponential Smoothing (?? = ", result$alpha, ")"),
         subtitle = paste0("April 2026 Forecast: ", round(result$next_forecast, 2)),
         x = "Year", y = "Volume Index (2021=100)") +
    ts_theme()
  save_plot(p, "exponential_smoothing_plot.png")
  p
}

# =============================================================================
# 5. HOLT'S TREND-ADJUSTED EXPONENTIAL SMOOTHING PLOT
# =============================================================================
plot_holt <- function(ts_data, result) {
  df <- build_plot_df(ts_data, result$fitted, result$next_forecast)
  label <- paste0("Holt (??=", result$alpha, ", ??=", result$beta, ")")
  p  <- ggplot(df, aes(x = t)) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 0.9, na.rm = TRUE) +
    geom_line(aes(y = fitted, color = label),
              linewidth = 0.8, linetype = "dashed", na.rm = TRUE) +
    geom_point(data = tail(df, 1),
               aes(y = fitted, color = "April 2026 Forecast"),
               size = 4, shape = 18) +
    scale_color_manual(values = setNames(
      c("#2C7BB6", "#D7191C", "#1A9641"),
      c("Actual",   label,     "April 2026 Forecast"))) +
    labs(title    = "Actual vs Trend-Adjusted Exp. Smoothing (Holt's Method)",
         subtitle = paste0("April 2026 Forecast: ", round(result$next_forecast, 2)),
         x = "Year", y = "Volume Index (2021=100)") +
    ts_theme()
  save_plot(p, "trend_adjusted_smoothing_plot.png")
  p
}

# =============================================================================
# 6. LINEAR TREND PROJECTION PLOT
# =============================================================================
plot_linear_trend <- function(ts_data, result) {
  df <- build_plot_df(ts_data, result$fitted, result$next_forecast)
  p  <- ggplot(df, aes(x = t)) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 0.9, na.rm = TRUE) +
    geom_line(aes(y = fitted, color = "Linear Trend"), linewidth = 0.8,
              linetype = "dashed", na.rm = TRUE) +
    geom_point(data = tail(df, 1),
               aes(y = fitted, color = "April 2026 Forecast"),
               size = 4, shape = 18) +
    scale_color_manual(values = c("Actual" = "#2C7BB6",
                                  "Linear Trend" = "#D7191C",
                                  "April 2026 Forecast" = "#1A9641")) +
    labs(title    = paste0("Actual vs Linear Trend Projection  |  ?? = ",
                           round(result$intercept, 2), " + ",
                           round(result$slope, 2), "??t"),
         subtitle = paste0("April 2026 Forecast: ", round(result$next_forecast, 2)),
         x = "Year", y = "Volume Index (2021=100)") +
    ts_theme()
  save_plot(p, "trend_projection_plot.png")
  p
}

# =============================================================================
# 7. SEASONAL INDICES PLOT
# =============================================================================
plot_seasonal_indices <- function(ts_data, result) {
  df <- build_plot_df(ts_data, result$fitted, result$next_forecast)
  p  <- ggplot(df, aes(x = t)) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 0.9, na.rm = TRUE) +
    geom_line(aes(y = fitted, color = "Seasonal Indices Forecast"),
              linewidth = 0.8, linetype = "dashed", na.rm = TRUE) +
    geom_point(data = tail(df, 1),
               aes(y = fitted, color = "April 2026 Forecast"),
               size = 4, shape = 18) +
    scale_color_manual(values = c("Actual" = "#2C7BB6",
                                  "Seasonal Indices Forecast" = "#D7191C",
                                  "April 2026 Forecast" = "#1A9641")) +
    labs(title    = "Actual vs Seasonal Indices Forecast",
         subtitle = paste0("April 2026 Forecast: ", round(result$next_forecast, 2)),
         x = "Year", y = "Volume Index (2021=100)") +
    ts_theme()
  save_plot(p, "seasonal_indices_plot.png")
  p
}

# =============================================================================
# 8. ADDITIVE DECOMPOSITION PLOT
# =============================================================================
plot_additive_decomp <- function(ts_data, result) {
  df <- build_plot_df(ts_data, result$fitted, result$next_forecast)
  p  <- ggplot(df, aes(x = t)) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 0.9, na.rm = TRUE) +
    geom_line(aes(y = fitted, color = "Additive Decomp. Forecast"),
              linewidth = 0.8, linetype = "dashed", na.rm = TRUE) +
    geom_point(data = tail(df, 1),
               aes(y = fitted, color = "April 2026 Forecast"),
               size = 4, shape = 18) +
    scale_color_manual(values = c("Actual" = "#2C7BB6",
                                  "Additive Decomp. Forecast" = "#D7191C",
                                  "April 2026 Forecast" = "#1A9641")) +
    labs(title    = "Actual vs Additive Decomposition Forecast",
         subtitle = paste0("April 2026 Forecast: ", round(result$next_forecast, 2)),
         x = "Year", y = "Volume Index (2021=100)") +
    ts_theme()
  save_plot(p, "additive_decomposition_plot.png")
  p
}

# =============================================================================
# 9. MULTIPLICATIVE DECOMPOSITION PLOT
# =============================================================================
plot_multiplicative_decomp <- function(ts_data, result) {
  df <- build_plot_df(ts_data, result$fitted, result$next_forecast)
  p  <- ggplot(df, aes(x = t)) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 0.9, na.rm = TRUE) +
    geom_line(aes(y = fitted, color = "Multiplicative Decomp. Forecast"),
              linewidth = 0.8, linetype = "dashed", na.rm = TRUE) +
    geom_point(data = tail(df, 1),
               aes(y = fitted, color = "April 2026 Forecast"),
               size = 4, shape = 18) +
    scale_color_manual(values = c("Actual" = "#2C7BB6",
                                  "Multiplicative Decomp. Forecast" = "#D7191C",
                                  "April 2026 Forecast" = "#1A9641")) +
    labs(title    = "Actual vs Multiplicative Decomposition Forecast",
         subtitle = paste0("April 2026 Forecast: ", round(result$next_forecast, 2)),
         x = "Year", y = "Volume Index (2021=100)") +
    ts_theme()
  save_plot(p, "multiplicative_decomposition_plot.png")
  p
}

# =============================================================================
# 10. REGRESSION WITH TREND AND SEASONAL DUMMIES PLOT
# =============================================================================
plot_regression_seasonal <- function(ts_data, result) {
  df <- build_plot_df(ts_data, result$fitted, result$next_forecast)
  p  <- ggplot(df, aes(x = t)) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 0.9, na.rm = TRUE) +
    geom_line(aes(y = fitted, color = "Regression Forecast"),
              linewidth = 0.8, linetype = "dashed", na.rm = TRUE) +
    geom_point(data = tail(df, 1),
               aes(y = fitted, color = "April 2026 Forecast"),
               size = 4, shape = 18) +
    scale_color_manual(values = c("Actual" = "#2C7BB6",
                                  "Regression Forecast" = "#D7191C",
                                  "April 2026 Forecast" = "#1A9641")) +
    labs(title    = "Actual vs Regression with Trend & Seasonal Dummies",
         subtitle = paste0("April 2026 Forecast: ", round(result$next_forecast, 2)),
         x = "Year", y = "Volume Index (2021=100)") +
    ts_theme()
  save_plot(p, "regression_seasonal_dummy_plot.png")
  p
}

# =============================================================================
# SUPERIOR METHOD FINAL PLOT
# =============================================================================
plot_superior <- function(ts_data, result, superior_name) {
  df <- build_plot_df(ts_data, result$fitted, result$next_forecast)
  
  p  <- ggplot(df, aes(x = t)) +
    geom_line(aes(y = actual, color = "Actual Series"), linewidth = 1.1, na.rm = TRUE) +
    geom_line(aes(y = fitted, color = "Fitted Values"),
              linewidth = 0.9, linetype = "dashed", na.rm = TRUE) +
    geom_point(data = tail(df, 1),
               aes(y = fitted),
               color = "#1A9641", size = 5, shape = 18) +
    annotate("text",
             x     = max(df$t, na.rm = TRUE),
             y     = tail(df$fitted[!is.na(df$fitted)], 1),
             label = paste0("April 2026\n",
                            round(tail(df$fitted[!is.na(df$fitted)], 1), 2)),
             hjust = 0, vjust = -0.5, color = "#1A9641", fontface = "bold", size = 3.5) +
    scale_color_manual(values = c("Actual Series" = "#2C7BB6",
                                  "Fitted Values" = "#D7191C")) +
    labs(title    = paste0("Superior Method: ", superior_name),
         subtitle = paste0("Final Forecast ??? April 2026 | ",
                           "T????K Retail Sales Volume Index: Computers & Telecom Equipment"),
         x        = "Year",
         y        = "Volume Index (2021=100)") +
    ts_theme() +
    theme(plot.title = element_text(color = "#1A9641", face = "bold", size = 14))
  
  save_plot(p, "superior_method_plot.png", width = 12, height = 5.5)
  p
}

# =============================================================================
# SEASONAL INDEX BAR CHART (supplementary)
# =============================================================================
plot_seasonal_index_bars <- function(seasonal_indices) {
  months <- c("Jan","Feb","Mar","Apr","May","Jun",
              "Jul","Aug","Sep","Oct","Nov","Dec")
  df <- data.frame(
    Month = factor(months, levels = months),
    Index = seasonal_indices
  )
  p <- ggplot(df, aes(x = Month, y = Index, fill = Index > 1)) +
    geom_bar(stat = "identity", color = "white") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey30") +
    scale_fill_manual(values = c("FALSE" = "#D7191C", "TRUE" = "#1A9641"),
                      labels = c("Below Average", "Above Average"),
                      name   = "") +
    labs(title    = "Monthly Seasonal Indices",
         subtitle = "Values > 1.0 indicate above-average retail activity",
         x = "Month", y = "Seasonal Index") +
    ts_theme()
  save_plot(p, "seasonal_index_barplot.png", width = 8, height = 4)
  p
}

# =============================================================================
# MASTER PLOT FUNCTION ??? generates all 12 required plots
# =============================================================================
generate_all_plots <- function(ts_data, methods_list, superior_method_name) {
  plot_actual_series(ts_data)
  plot_naive(ts_data,              methods_list$naive)
  plot_ma(ts_data,                 methods_list$ma)
  plot_wma(ts_data,                methods_list$wma)
  plot_ses(ts_data,                methods_list$ses)
  plot_holt(ts_data,               methods_list$holt)
  plot_linear_trend(ts_data,       methods_list$linear_trend)
  plot_seasonal_indices(ts_data,   methods_list$seas_idx)
  plot_additive_decomp(ts_data,    methods_list$add_decomp)
  plot_multiplicative_decomp(ts_data, methods_list$mult_decomp)
  plot_regression_seasonal(ts_data, methods_list$reg_seasonal)
  plot_seasonal_index_bars(methods_list$seas_idx$seasonal_indices)
  
  # Identify the result object for the superior method
  superior_key <- switch(superior_method_name,
                         "Naive Forecasting"                        = "naive",
                         "Moving Average (k=3)"                     = "ma",
                         "Weighted Moving Average"                  = "wma",
                         "Exponential Smoothing"                    = "ses",
                         "Trend-Adjusted Exp. Smoothing (Holt)"     = "holt",
                         "Linear Trend Projection"                  = "linear_trend",
                         "Seasonal Indices"                         = "seas_idx",
                         "Additive Decomposition"                   = "add_decomp",
                         "Multiplicative Decomposition"             = "mult_decomp",
                         "Regression (Trend + Seasonal Dummies)"    = "reg_seasonal",
                         "reg_seasonal"   # default fallback
  )
  
  superior_result <- methods_list[[superior_key]]
  plot_superior(ts_data, superior_result, superior_method_name)
  
  cat("\nAll plots saved to", FIGURES_DIR, "\n")
}