#' 
#' @param data A dataframe containing daily measurements of minimum & maximum temperatures (Celcius) and precipitation (mm).
#' @param baseline_yield Expected, baseline amount of almond yield, based on arbitrary historical average (tons per acre).
#' @param unit_price Unit price for almonds ($ per ton).
#' @param unit_price_uncertain Uncertainty for unit price as a percent in decimal form, to be added to unit_price to simulate variance in sell prices.
#' @param unit_cost Unit cost for almonds ($ per ton).
#' @param unit_cost_uncertain Uncertainty for unit cost as a percent in decimal form, to be added to unit_cost to simulate variance in costs.
#' 
#' @return A dataframe containing year, amount of yield per acre, resulting unit price, resulting unit cost, and profit, where each observation is a simulated year of specified almond market conditions.

calculate_almond_profit <- function(data, baseline_yield = 200, unit_price = 5000, unit_price_uncertain = 0.10, unit_cost = 3000, unit_cost_uncertain = 0.10) {

  # Yearly Minimum Feb temps
  feb_temps_yearly <- data |> 
  filter(month == 2) |> 
  group_by(year) |> 
  summarise(feb_min_temps = min(tmin_c)) |> 
  pull(feb_min_temps)

  # Yearly January precip
  jan_precip_yearly <- data |> 
  filter(month == 1) |> 
  group_by(year) |> 
  summarise(jan_precip_sum = sum(precip)) |> 
  pull(jan_precip_sum)

  # Extract years
  years <- data |> 
      filter(month == 1) |> 
      pull(year) |> 
      unique()
  
  # Calculate anomaly yields
  yield_anomalies <- -0.015 * feb_temps_yearly - 0.0046 * feb_temps_yearly^2 - 0.07 * jan_precip_yearly + 0.0043 * jan_precip_yearly^2 + 0.28

  # Find total yields
  total_yields <- baseline_yield + yield_anomalies

  # Add varying uncertainty to unit cost
  varying_unit_cost <- unit_cost + rnorm(n = length(years), mean = 0, sd = unit_cost * unit_cost_uncertain)

  # Add varying unceratinty to unit price
  varying_unit_price <- unit_price + rnorm(n = length(years), mean = 0, sd = unit_price * unit_price_uncertain)

  # Find profit
  # profit = total yield * (revenue - cost)
  profit <- total_yields * (varying_unit_price - varying_unit_cost)

  # Output dataframe
  data.frame(
  year = years,
  yield_ton_acre = total_yields,
  unit_price_dollar_ton = varying_unit_price,
  unit_cost_dollar_ton = varying_unit_cost,
  profit_dollar_acre = profit
  )

}