# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_ceilings_floors_xml
#'
#' Construct XML data structure for \code{aeei.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_ceilings_floors.xml}.
module_policy_ceilings_floors_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L301.policy_port_stnd",
             "L301.policy_RES_coefs",
             "L301.RES_secout",
             "L301.input_tax"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "policy_ceilings_floors.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L301.policy_port_stnd <- get_data(all_data, "L301.policy_port_stnd")
    L301.policy_RES_coefs <- get_data(all_data, "L301.policy_RES_coefs")
    L301.RES_secout <- get_data(all_data, "L301.RES_secout")
    L301.input_tax <- get_data(all_data, "L301.input_tax")
    # ===================================================

    # Produce outputs
    create_xml("policy_ceilings_floors.xml") %>%
      add_xml_data(L301.policy_port_stnd, "PortfolioStdConstraint") %>%
      add_xml_data(L301.policy_RES_coefs, "StubTechCoefIndUrb") %>%
      add_xml_data(L301.RES_secout, "TechResSecOut") %>%
      add_xml_data(L301.input_tax, "TechInputTax") %>%
      add_precursors("L301.policy_port_stnd",
                     "L301.policy_RES_coefs",
                     "L301.RES_secout",
                     "L301.input_tax") ->
      policy_ceilings_floors.xml

    return_data(policy_ceilings_floors.xml)
  } else {
    stop("Unknown command")
  }
}
