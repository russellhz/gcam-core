# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L354.FuelStandards
#'
#' Produce fuel standards coefficients
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L354.FuelStandards}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L354.FuelStandards <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_FuelStandards",
             "L254.StubTranTechCoef"
    ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L354.FuelStandards"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_FuelStandards <- get_data(all_data, "policy/A_FuelStandards")
    L254.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef")

    # Convert to long
    L354.FuelStandards <- A_FuelStandards %>%
      gather_years(value_col = "coefficient") %>%
      mutate(market.name = region) %>%
      # Join in default GCAM coefficients and select the minimum coefficient
      left_join_error_no_match(L254.StubTranTechCoef,
                               by = c("region", "supplysector", "tranSubsector", "stub.technology",
                                      "minicam.energy.input", "year",  "market.name",
                                      "SSP_sce" = "sce")) %>%
      mutate(coefficient = pmin(coefficient.x, coefficient.y)) %>%
      select(xml, LEVEL2_DATA_NAMES[["StubTranTechCoef"]])

    # Produce outputs
    L354.FuelStandards %>%
      add_title("Policy fuel standards", overwrite = T) %>%
      add_units("BTU/vkm") %>%
      add_precursors("policy/A_FuelStandards",
                     "L254.StubTranTechCoef") ->
      L354.FuelStandards

    return_data(L354.FuelStandards)
  } else {
    stop("Unknown command")
  }
}

