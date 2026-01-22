#' Calculate the minimum, mean, and maximum almond yield
#' 
#' @param data A dataframe containing daily measurements of minimum & maximum temperatures (Celcius) and precipitation (mm).
#' 
#' @return A kable table with the resulting almond yields.
calculate_almond_yield <- function(data){
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

yields <- -0.015 * feb_temps_yearly - 0.0046 * feb_temps_yearly^2 - 0.07 * jan_precip_yearly + 0.0043 * jan_precip_yearly^2 + 0.28
  
min_yield <- min(yields)
max_yield <- max(yields)
mean_yield <- mean(yields)
  
output_df <- data.frame(crop = "almond", 
    min_yield = round(min_yield, 3),
    mean_yield = round(mean_yield, 3), 
    max_yield = round(max_yield, 3))

kableExtra::kable(output_df, col.names = c("Crop", "Min. Yield", "Mean Yield", "Max Yield"))
} 