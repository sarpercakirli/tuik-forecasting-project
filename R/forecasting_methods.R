# =============================================================================
# R/forecasting_methods.R
# Purpose: Implement all required forecasting methods and return a named list
#          containing fitted values, next-period forecast, and forecast errors.
# Author : Sarper | MIS, Marmara University
# Course : Quantitative Analysis for Decision Making
# =============================================================================

library(forecast)
library(dplyr)

# =============================================================================
# HELPER: align actual and fitted values (drop leading NAs from fitted)
# =============================================================================
align_errors <- function(actual, fitted) {
  # Returns a data.frame with matching actual / fitted rows only
  n <- length(actual)
  m <- length(fitted)
  if (m < n) {
    # fitted is shorter ??? pad with NA at the front
    fitted <- c(rep(NA, n - m), fitted)
  }
  valid <- !is.na(fitted) & !is.na(actual)
  data.frame(
    actual = as.numeric(actual)[valid],
    fitted = as.numeric(fitted)[valid],
    error  = as.numeric(actual)[valid] - as.numeric(fitted)[valid]
  )
}

# =============================================================================
# 1. NA??VE FORECASTING
#    Forecast(t) = Actual(t-1)
# =============================================================================
run_naive <- function(ts_data) {
  n       <- length(ts_data)
  actual  <- as.numeric(ts_data)
  
  # Fitted: shift actual forward by 1 period
  fitted_vals <- c(NA, actual[-n])
  
  # Next-period (April 2026) forecast = last observed value (March 2026)
  next_forecast <- actual[n]
  
  ae <- align_errors(actual, fitted_vals)
  
  list(
    method        = "Naive Forecasting",
    fitted        = fitted_vals,
    next_forecast = next_forecast,
    errors        = ae$error,
    actual_match  = ae$actual,
    fitted_match  = ae$fitted
  )
}

# =============================================================================
# 2. MOVING AVERAGE (window = 3 months)
#    Window size 3 is justified: the series has strong trend + seasonality.
#    A narrow window (3) tracks the trend without over-smoothing seasonal peaks.
# =============================================================================
run_ma <- function(ts_data, window = 3) {
  n      <- length(ts_data)
  actual <- as.numeric(ts_data)
  
  # Centred moving average for fit; one-step-ahead for forecast
  fitted_vals <- stats::filter(actual, rep(1 / window, window), sides = 1)
  fitted_vals <- as.numeric(fitted_vals)          # sides=1 => trailing MA
  
  # Next-period forecast = MA of last `window` observations
  next_forecast <- mean(actual[(n - window + 1):n])
  
  ae <- align_errors(actual, fitted_vals)
  
  list(
    method        = paste0("Moving Average (k=", window, ")"),
    window        = window,
    fitted        = fitted_vals,
    next_forecast = next_forecast,
    errors        = ae$error,
    actual_match  = ae$actual,
    fitted_match  = ae$fitted
  )
}

# =============================================================================
# 3. WEIGHTED MOVING AVERAGE (weights = 0.5, 0.3, 0.2 for t-1, t-2, t-3)
#    Higher weights assigned to more recent periods.
# =============================================================================
run_wma <- function(ts_data, weights = c(0.5, 0.3, 0.2)) {
  # weights[1] is the most recent, weights[k] is the oldest
  w      <- weights / sum(weights)          # normalise just in case
  k      <- length(w)
  n      <- length(ts_data)
  actual <- as.numeric(ts_data)
  
  fitted_vals <- rep(NA, n)
  for (i in (k + 1):n) {
    fitted_vals[i] <- sum(w * actual[(i - 1):(i - k)])
  }
  
  # Next-period forecast
  next_forecast <- sum(w * actual[n:(n - k + 1)])
  
  ae <- align_errors(actual, fitted_vals)
  
  list(
    method        = "Weighted Moving Average",
    weights       = weights,
    fitted        = fitted_vals,
    next_forecast = next_forecast,
    errors        = ae$error,
    actual_match  = ae$actual,
    fitted_match  = ae$fitted
  )
}

# =============================================================================
# 4. SIMPLE EXPONENTIAL SMOOTHING
#    alpha chosen by minimising SSE via forecast::ets()
# =============================================================================
run_ses <- function(ts_data) {
  model  <- forecast::ets(ts_data, model = "ANN")   # Additive, No trend, No season
  alpha  <- model$par["alpha"]
  fitted_vals <- as.numeric(fitted(model))
  next_forecast <- as.numeric(forecast(model, h = 1)$mean)
  
  ae <- align_errors(as.numeric(ts_data), fitted_vals)
  
  list(
    method        = "Exponential Smoothing",
    alpha         = round(alpha, 4),
    model         = model,
    fitted        = fitted_vals,
    next_forecast = next_forecast,
    errors        = ae$error,
    actual_match  = ae$actual,
    fitted_match  = ae$fitted
  )
}

# =============================================================================
# 5. TREND-ADJUSTED EXPONENTIAL SMOOTHING (HOLT'S METHOD)
#    Both alpha and beta optimised by ets(); model = "AAN"
#    Applicable: series shows strong upward trend ??? Holt's method is appropriate.
# =============================================================================
run_holt <- function(ts_data) {
  model  <- forecast::ets(ts_data, model = "AAN")   # Additive Error, Additive Trend
  alpha  <- model$par["alpha"]
  beta   <- model$par["beta"]
  fitted_vals   <- as.numeric(fitted(model))
  next_forecast <- as.numeric(forecast(model, h = 1)$mean)
  
  ae <- align_errors(as.numeric(ts_data), fitted_vals)
  
  list(
    method        = "Trend-Adjusted Exp. Smoothing (Holt)",
    alpha         = round(alpha, 4),
    beta          = round(beta, 4),
    model         = model,
    fitted        = fitted_vals,
    next_forecast = next_forecast,
    errors        = ae$error,
    actual_match  = ae$actual,
    fitted_match  = ae$fitted
  )
}

# =============================================================================
# 6. LINEAR TREND PROJECTION (OLS on time index)
# =============================================================================
run_linear_trend <- function(ts_data) {
  n      <- length(ts_data)
  actual <- as.numeric(ts_data)
  t_idx  <- seq_len(n)
  
  mdl    <- lm(actual ~ t_idx)
  coefs  <- coef(mdl)
  fitted_vals <- as.numeric(fitted(mdl))
  
  # Next period = t + 1
  next_t        <- n + 1
  next_forecast <- coefs[1] + coefs[2] * next_t
  
  ae <- align_errors(actual, fitted_vals)
  
  list(
    method        = "Linear Trend Projection",
    intercept     = round(coefs[1], 4),
    slope         = round(coefs[2], 4),
    model         = mdl,
    fitted        = fitted_vals,
    next_forecast = next_forecast,
    errors        = ae$error,
    actual_match  = ae$actual,
    fitted_match  = ae$fitted
  )
}

# =============================================================================
# 7. SEASONAL INDICES METHOD
#    Steps: (1) compute seasonal indices from ratio-to-moving-average
#           (2) deseasonalise ??? fit linear trend ??? reseasonalise
# =============================================================================
run_seasonal_indices <- function(ts_data) {
  freq   <- frequency(ts_data)      # 12 for monthly
  n      <- length(ts_data)
  actual <- as.numeric(ts_data)
  
  # --- 7a. Centred moving average (order = freq)
  cma <- as.numeric(stats::filter(actual, rep(1 / freq, freq), sides = 2))
  
  # --- 7b. Seasonal ratios
  ratios <- actual / cma
  
  # --- 7c. Average seasonal index per month
  s_idx <- numeric(freq)
  for (m in 1:freq) {
    positions <- seq(m, n, by = freq)
    valid     <- positions[!is.na(ratios[positions])]
    s_idx[m]  <- mean(ratios[valid], na.rm = TRUE)
  }
  # Normalise so indices sum to freq
  s_idx <- s_idx * (freq / sum(s_idx))
  
  # --- 7d. Deseasonalised series
  month_seq    <- cycle(ts_data)
  deseas_vals  <- actual / s_idx[month_seq]
  
  # --- 7e. Fit linear trend to deseasonalised data
  t_idx  <- seq_len(n)
  trend_mdl <- lm(deseas_vals ~ t_idx)
  trend_fit <- as.numeric(fitted(trend_mdl))
  
  # --- 7f. Reseasonalise fitted values
  fitted_vals <- trend_fit * s_idx[month_seq]
  
  # --- 7g. Next-period forecast (April = month 4)
  next_t     <- n + 1
  next_month <- ((start(ts_data)[2] - 1 + n) %% 12) + 1   # month of next period
  trend_next <- coef(trend_mdl)[1] + coef(trend_mdl)[2] * next_t
  next_forecast <- trend_next * s_idx[next_month]
  
  ae <- align_errors(actual, fitted_vals)
  
  list(
    method           = "Seasonal Indices",
    seasonal_indices = round(s_idx, 4),
    fitted           = fitted_vals,
    next_forecast    = next_forecast,
    next_month       = next_month,
    errors           = ae$error,
    actual_match     = ae$actual,
    fitted_match     = ae$fitted
  )
}

# =============================================================================
# 8. ADDITIVE DECOMPOSITION
#    Uses stats::decompose(); refitted values = trend + seasonal
# =============================================================================
run_additive_decomp <- function(ts_data) {
  dec  <- decompose(ts_data, type = "additive")
  
  trend_comp    <- as.numeric(dec$trend)
  seasonal_comp <- as.numeric(dec$seasonal)
  random_comp   <- as.numeric(dec$random)
  
  # Fitted = trend + seasonal (ignores random/residual)
  fitted_vals <- trend_comp + seasonal_comp
  
  # For next-period forecast: extend trend linearly and add seasonal index
  n          <- length(ts_data)
  actual     <- as.numeric(ts_data)
  t_idx      <- seq_len(n)
  valid_t    <- !is.na(trend_comp)
  trend_lm   <- lm(trend_comp[valid_t] ~ t_idx[valid_t])
  next_trend <- as.numeric(coef(trend_lm)[1] + coef(trend_lm)[2] * (n + 1))
  
  # Seasonal component for April
  next_month    <- ((start(ts_data)[2] - 1 + n) %% 12) + 1
  s_vals        <- dec$seasonal[1:12]
  next_seasonal <- s_vals[next_month]
  next_forecast <- next_trend + next_seasonal
  
  ae <- align_errors(actual, fitted_vals)
  
  list(
    method           = "Additive Decomposition",
    decompose_object = dec,
    trend            = trend_comp,
    seasonal         = seasonal_comp,
    random           = random_comp,
    fitted           = fitted_vals,
    next_forecast    = next_forecast,
    errors           = ae$error,
    actual_match     = ae$actual,
    fitted_match     = ae$fitted
  )
}

# =============================================================================
# 9. MULTIPLICATIVE DECOMPOSITION
#    Uses stats::decompose(); refitted values = trend * seasonal
# =============================================================================
run_multiplicative_decomp <- function(ts_data) {
  dec  <- decompose(ts_data, type = "multiplicative")
  
  trend_comp    <- as.numeric(dec$trend)
  seasonal_comp <- as.numeric(dec$seasonal)
  random_comp   <- as.numeric(dec$random)
  
  fitted_vals <- trend_comp * seasonal_comp
  
  # Extend trend linearly
  n         <- length(ts_data)
  actual    <- as.numeric(ts_data)
  t_idx     <- seq_len(n)
  valid_t   <- !is.na(trend_comp)
  trend_lm  <- lm(trend_comp[valid_t] ~ t_idx[valid_t])
  next_trend <- as.numeric(coef(trend_lm)[1] + coef(trend_lm)[2] * (n + 1))
  
  next_month    <- ((start(ts_data)[2] - 1 + n) %% 12) + 1
  s_vals        <- dec$seasonal[1:12]
  next_seasonal <- s_vals[next_month]
  next_forecast <- next_trend * next_seasonal
  
  ae <- align_errors(actual, fitted_vals)
  
  list(
    method           = "Multiplicative Decomposition",
    decompose_object = dec,
    trend            = trend_comp,
    seasonal         = seasonal_comp,
    random           = random_comp,
    fitted           = fitted_vals,
    next_forecast    = next_forecast,
    errors           = ae$error,
    actual_match     = ae$actual,
    fitted_match     = ae$fitted
  )
}

# =============================================================================
# 10. REGRESSION WITH TREND AND SEASONAL DUMMY VARIABLES
#     Includes a time index (t) and 11 monthly dummy variables (January omitted).
# =============================================================================
run_regression_trend_seasonal <- function(ts_data) {
  n         <- length(ts_data)
  actual    <- as.numeric(ts_data)
  t_idx     <- seq_len(n)
  month_seq <- as.integer(cycle(ts_data))
  
  # Build dummy matrix (January = reference category, 11 dummies)
  dummies <- model.matrix(~ factor(month_seq))[, -1]
  colnames(dummies) <- paste0("M", 2:12)
  
  df_reg <- data.frame(y = actual, t = t_idx, dummies)
  mdl    <- lm(y ~ ., data = df_reg)
  
  fitted_vals <- as.numeric(fitted(mdl))
  
  # Next period is April 2026 (month 4)
  next_t      <- n + 1
  next_month  <- ((start(ts_data)[2] - 1 + n) %% 12) + 1   # should be 4
  
  new_dummies           <- setNames(rep(0, 11), paste0("M", 2:12))
  if (next_month >= 2) new_dummies[paste0("M", next_month)] <- 1
  new_row   <- data.frame(t = next_t, t(new_dummies))
  next_forecast <- as.numeric(predict(mdl, newdata = new_row))
  
  ae <- align_errors(actual, fitted_vals)
  
  list(
    method        = "Regression (Trend + Seasonal Dummies)",
    model         = mdl,
    coefs         = coef(mdl),
    fitted        = fitted_vals,
    next_forecast = next_forecast,
    next_month    = next_month,
    errors        = ae$error,
    actual_match  = ae$actual,
    fitted_match  = ae$fitted
  )
}

# =============================================================================
# MASTER RUNNER ??? calls all 10 methods and returns a named list
# =============================================================================
run_all_methods <- function(ts_data) {
  list(
    naive        = run_naive(ts_data),
    ma           = run_ma(ts_data, window = 3),
    wma          = run_wma(ts_data, weights = c(0.5, 0.3, 0.2)),
    ses          = run_ses(ts_data),
    holt         = run_holt(ts_data),
    linear_trend = run_linear_trend(ts_data),
    seas_idx     = run_seasonal_indices(ts_data),
    add_decomp   = run_additive_decomp(ts_data),
    mult_decomp  = run_multiplicative_decomp(ts_data),
    reg_seasonal = run_regression_trend_seasonal(ts_data)
  )
}