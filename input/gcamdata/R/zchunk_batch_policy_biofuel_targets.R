# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_biofuel_targets_xml
#'
#' Construct XML data structure for \code{aeei.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{biofuel_targets.xml}.
module_policy_biofuel_targets_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L322.policy_port_stnd",
             "L322.biofuel_policy_coefs",
             "L322.biofuel_secout"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "policy_biofuel_targets.xml"))
  } else if(command == driver.MAKE) {
    
    all_data <- list(...)[[1]]
    
    # Load required inputs
    L322.policy_port_stnd <- get_data(all_data, "L322.policy_port_stnd")
    L322.biofuel_policy_coefs <- get_data(all_data, "L322.biofuel_policy_coefs")
    L322.biofuel_secout <- get_data(all_data, "L322.biofuel_secout")
    # ===================================================
    
    # Produce outputs
    create_xml("policy_biofuel_targets.xml") %>%
      add_xml_data(L322.policy_port_stnd, "PortfolioStdConstraint") %>%
      add_xml_data(L322.biofuel_policy_coefs, "StubTechCoefIndUrb") %>% 
      add_xml_data(L322.biofuel_secout, "TechResSecOut") %>%
      add_precursors("L322.policy_port_stnd",
                     "L322.biofuel_policy_coefs",
                     "L322.biofuel_secout") ->
      policy_biofuel_targets.xml
    
    return_data(policy_biofuel_targets.xml)
  } else {
    stop("Unknown command")
  }
}
