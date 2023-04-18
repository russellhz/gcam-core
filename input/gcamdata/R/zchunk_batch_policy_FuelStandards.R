# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_FuelStandards_xml
#'
#' Construct XML data structure for \code{aeei.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_FuelStandards.xml}.
module_policy_FuelStandards_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L354.FuelStandards"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "policy_FuelStandards.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L354.FuelStandards <- get_data(all_data, "L354.FuelStandards")
    # ===================================================

    # Produce outputs
    create_xml("policy_FuelStandards.xml") %>%
      add_xml_data(L354.FuelStandards, "StubTranTechCoef") %>%
      add_precursors("L354.FuelStandards") ->
      policy_FuelStandards.xml

    return_data(policy_FuelStandards.xml)
  } else {
    stop("Unknown command")
  }
}
