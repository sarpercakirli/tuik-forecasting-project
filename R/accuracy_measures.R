# =============================================================================
# R/accuracy_measures.R
# Purpose: Calculate all required forecast accuracy and monitoring measures
#          for every candidate method, compile a comparison data frame,
#          and identify the superior method.
# Author : Sarper | MIS, Marmara University
# Course : Quantitative Analysis for Decision Making
# =============================================================================

library(dplyr)

# =============================================================================
# calc_accuracy()
# Given a vector of forecast errors, compute the seven required statistics.
# =============================================================================
calc_accuracy <- function(errors, actual) {
  # Remove NAs (leading periods with no forecast)
  errors <- errors[!is.na(errors)]
  actual <- actual[!is.na(errors)]   # keep matching actuals
  
  n      <- length(errors)
  if (n == 0) return(rep(NA, 7))
  
  bias            <- mean(errors)                            # Mean Error
  mad             <- mean(abs(errors))                       # MAD
  mse             <- mean(errors^2)                          # MSE
  mape            <- mean(abs(errors / actual)) * 100        # MAPE (%)
  rsfe            <- sum(errors)                             # Running Sum of Forecast Errors
  tracking_signal <- rsfe / mad                              # Tracking Signal
  
  c(Bias = round(bias, 4),
    MAD  = round(mad,  4),
    MSE  = round(mse,  4),
    MAPE = round(mape, 4),
    RSFE = round(rsfe, 4),
    TS   = round(tracking_signal, 4))
}

# =============================================================================
# build_accuracy_table()
# Loops over all method results from run_all_methods() and compiles a
# single data.frame; also appends the next-period forecast.
# =============================================================================
build_accuracy_table <- function(methods_list) {
  
  rows <- lapply(names(methods_list), function(nm) {
    res    <- methods_list[[nm]]
    errors <- res$errors
    actual <- res$actual_match
    
    # Handle potential NA errors (methods that partially failed)
    if (is.null(errors) || all(is.na(errors))) {
      stats <- setNames(rep(NA, 6),
                        c("Bias", "MAD", "MSE", "MAPE", "RSFE", "TS"))
    } else {
      # Align actual with non-NA errors
      valid  <- !is.na(errors)
      stats  <- calc_accuracy(errors[valid], actual[valid])
    }
    
    data.frame(
      Method           = res$method,
      Bias_ME          = stats["Bias"],
      MAD              = stats["MAD"],
      MSE              = stats["MSE"],
      MAPE_pct         = stats["MAPE"],
      RSFE             = stats["RSFE"],
      Tracking_Signal  = stats["TS"],
      Next_Period_Fcst = round(res$next_forecast, 2),
      stringsAsFactors = FALSE,
      row.names        = NULL
    )
  })
  
  accuracy_df <- bind_rows(rows)
  rownames(accuracy_df) <- NULL
  return(accuracy_df)
}

# =============================================================================
# select_superior()
# Identifies the superior method using a composite score:
#   - Lowest MAPE (primary criterion)
#   - Tracking Signal closest to 0 (secondary; indicates no systematic bias)
#   - Visual confirmation from plot structure (documented in notebook)
# The Regression with Trend + Seasonal Dummies is the preferred method when:
#   (a) the series has a significant upward trend, AND
#   (b) strong seasonal patterns are present, AND
#   (c) its accuracy metrics are competitive.
# =============================================================================
select_superior <- function(accuracy_df) {
  # Exclude any rows with NA MAPE (inapplicable methods)
  valid_df <- accuracy_df %>%
    filter(!is.na(MAPE_pct)) %>%
    mutate(
      norm_MAPE = (MAPE_pct - min(MAPE_pct, na.rm = TRUE)) /
        (max(MAPE_pct, na.rm = TRUE) - min(MAPE_pct, na.rm = TRUE) + 1e-9),
      norm_TS   = abs(Tracking_Signal) / (max(abs(Tracking_Signal), na.rm = TRUE) + 1e-9),
      composite = 0.7 * norm_MAPE + 0.3 * norm_TS     # lower is better
    )
  
  best_row <- valid_df %>%
    slice_min(composite, n = 1, with_ties = FALSE)
  
  cat("\n============================================================\n")
  cat("SUPERIOR METHOD SELECTION\n")
  cat("============================================================\n")
  cat("Selected method     :", best_row$Method, "\n")
  cat("MAPE (%)            :", round(best_row$MAPE_pct, 3), "\n")
  cat("MAD                 :", round(best_row$MAD, 3), "\n")
  cat("Tracking Signal     :", round(best_row$Tracking_Signal, 3), "\n")
  cat("Next-Period Forecast:", round(best_row$Next_Period_Fcst, 2), "\n")
  cat("============================================================\n\n")
  
  return(best_row$Method)
}

# =============================================================================
# save_accuracy_table()
# Exports the accuracy data frame to outputs/tables/accuracy_comparison.csv
# =============================================================================
save_accuracy_table <- function(accuracy_df,
                                path = "outputs/tables/accuracy_comparison.csv") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(accuracy_df, path, row.names = FALSE)
  cat("Accuracy table saved to:", path, "\n")
}

# =============================================================================
# save_final_forecast()
# Exports the final forecast result to outputs/tables/final_forecast.csv
# =============================================================================
save_final_forecast <- function(superior_method,
                                forecast_value,
                                accuracy_df,
                                path = "outputs/tables/final_forecast.csv") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  acc_row <- accuracy_df %>%
    filter(Method == superior_method)
  
  final_df <- data.frame(
    Superior_Method         = superior_method,
    Data_Access_Date        = format(Sys.Date(), "%d %B %Y"),
    Latest_TUiK_Observation = "March 2026",
    Forecast_Target_Period  = "April 2026",
    Forecasted_Value        = round(forecast_value, 2),
    MAPE_pct                = round(acc_row$MAPE_pct, 3),
    MAD                     = round(acc_row$MAD, 3),
    MSE                     = round(acc_row$MSE, 3),
    Tracking_Signal         = round(acc_row$Tracking_Signal, 3),
    stringsAsFactors        = FALSE
  )
  
  write.csv(final_df, path, row.names = FALSE)
  cat("Final forecast saved to:", path, "\n")
  return(final_df)
}