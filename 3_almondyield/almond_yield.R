
# Define crop function
calculate_almond_yield <- function(data){

# Minimum Feb temps
feb_temps <- climate_df |> 
    filter(month == 2) |> 
    group_by(year) |> 
    summarise(feb_min_temp = min(tmin_c))

# January precip
jan_precip <- climate_df |> 
    filter(month == 1) |> 
    group_by(year) |> 
    summarise(jan_precip = sum(precip))

yields <- -0.015 * feb_temps$feb_min_temp - 0.0046 * feb_temps$feb_min_temp^2 - 0.07 * jan_precip$jan_precip + 0.0043 * jan_precip$jan_precip^2 + 0.28
  
min_yield <- min(yields)
max_yield <- max(yields)
mean_yield <- mean(yields)
  
output_df <- data.frame(crop = "almond", min_yield = round(min_yield, 3), max_yield = round(max_yield, 3), mean_yield = round(mean_yield, 3))

kableExtra::kable(output_df)
} 