#' 
#' @param baseline_yield Expected, baseline amount of almond yield, based on arbitrary historical average (pounds).
#' @param yield_anomaly 
#' @param unit_price

calculate_almond_profit <- function(baseline_yield = 500, yield_anomaly = 0, unit_price = 3) {

  # Assign anomaly df
  anomaly_df <- calculate_almond_yield()
  # Assign mean value from df
  mean_anomaly <- anomaly_df$mean_yield

}