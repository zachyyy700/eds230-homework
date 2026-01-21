
# Define crop function
calculate_crop_yield <- function(data, crop = "almond"){
if (crop == "almond"){
# Minimum Feb temps
feb_temps <- data |> 
    filter(month == 2) |> 
    group_by(year) |> 
    summarise(feb_min_temp = min(tmin_c)) |> 
    pull(feb_min_temp)

# January precip
jan_precip <- data |> 
    filter(month == 1) |> 
    group_by(year) |> 
    summarise(jan_precip = sum(precip)) |> 
    pull(jan_precip)

yields <- -0.015 * feb_temps - 0.0046 * feb_temps^2 - 0.07 * jan_precip + 0.0043 * jan_precip^2 + 0.28

} else if (crop == "wine grapes"){
# Minimum April temps
april_temps <- data |> 
    filter(month == 4) |> 
    group_by(year) |> 
    summarise(apr_min_temp = min(tmin_c)) |> 
    pull(apr_min_temp)
# June precip
june_precip <- data |> 
    filter(month == 6) |> 
    group_by(year) |> 
    summarise(june_precip = sum(precip)) |> 
    pull(june_precip)
# September precip
sep_precip <- data |> 
    filter(month == 9) |> 
    group_by(year) |> 
    summarise(sep_precip = sum(precip)) |> 
    pull(sep_precip)

yields <- 2.65 * april_temps - 0.17 * april_temps^2 + 4.78 * june_precip - 4.93 * june_precip^2 -2.24 * sep_precip + 1.54 * sep_precip^2 - 10.50

}
  
  
min_yield <- min(yields)
max_yield <- max(yields)
mean_yield <- mean(yields)

output_df <- data.frame(crop = crop, min_yield = round(min_yield, 3), max_yield = round(max_yield, 3), mean_yield = round(mean_yield, 3))

kableExtra::kable(output_df)
} 