#' Calculate the energy produced by a solar panel system in kilowatt-hours (kWh).
#'
#' @param a Solar panel area in meters squared (m^2).
#' @param r Panel yield performance (0-1), default value of 0.2, unitless.
#' @param pr Performance ratio (0-1), default value of 0.75, unitless.
#' @param h Annual average radiation of a panel system location in kilowatt-hours per meters squared (kWh m^-2).
#' 
#' @return Character string of energy produced by a panel system with units.
#' 
#' @examples
#' calculate_solarpanel_energy(a = 10, h = 100)
calculate_solarpanel_energy <- function(a, r = 0.25, pr = 0.75, h){
  # Calculate energy
  energy = a * r * pr * h

  # Return string with result
  return(paste0(energy, "kWh"))
}

