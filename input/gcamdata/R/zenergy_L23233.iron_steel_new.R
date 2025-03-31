# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L23233.iron_steel_new
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for new iron sector
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for iron sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr gather spread
#' @author Russell Horowitz Feb 26 2025
module_energy_L23233.iron_steel_new <- function(command, ...) {

  MODULE_INPUTS <- c(FILE = "energy/A3233.globaltech_coef")

  MODULE_OUTPUTS <- c("L23233.GlobalTechCoef_iron_steel_new")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ---------------------
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = T)

    # 2. Global Technology information --------------------
    # L23231.GlobalTechCoef_iron: Energy inputs and coefficients of iron technologies
    A3233.globaltech_coef %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, value, rule = 2),
             coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L23233.GlobalTechCoef_iron_steel_new


    # Produce outputs ===================================================
    L23233.GlobalTechCoef_iron_steel_new %>%
      add_title("Energy inputs and coefficients of iron technologies") %>%
      add_units("scrap input is unitless (Mt scrap per Mt steel); all others are GJ per kg (EJ of energy per Mt of steel)") %>%
      add_precursors("energy/A3233.globaltech_coef") ->
      L23233.GlobalTechCoef_iron_steel_new


    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
