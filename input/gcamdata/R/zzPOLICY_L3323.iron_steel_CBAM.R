# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L3323.iron_steel_CBAM
#'
#' Iron steel CBAM emissions
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L3323.iron_steel_CBAM <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L238.TechCoef_reg"
    ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L3323.iron_steel_CBAM_emiss"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    HIGH_CARBON_COEFFICIENT <- 0.455
    MED_CARBON_COEFFICIENT <- 0.153
    # Load required inputs
    L238.TechCoef_reg <- get_data(all_data, "L238.TechCoef_reg")

    # Get all region/tech/year combos
    L3323.iron_steel_CBAM_emiss <- L238.TechCoef_reg %>%
      filter(grepl("import", subsector),
              technology != "imported low-carbon iron and steel") %>%
      distinct(region, supplysector, subsector, stub.technology = technology, year) %>%
      mutate(Non.CO2 = "iron.steel.trade.CBAM",
             emiss.coeff = case_when(
               stub.technology == "imported high-carbon iron and steel" ~ HIGH_CARBON_COEFFICIENT,
               stub.technology == "imported med-carbon iron and steel" ~ MED_CARBON_COEFFICIENT),
               emissions.unit = "Mt")

    # Produce outputs
    L3323.iron_steel_CBAM_emiss %>%
      add_title("Iron steel import CO2 coefficients", overwrite = T) %>%
      add_units("MT CO2") %>%
      add_precursors("L238.TechCoef_reg") ->
      L3323.iron_steel_CBAM_emiss

    return_data(L3323.iron_steel_CBAM_emiss)
  } else {
    stop("Unknown command")
  }
}

