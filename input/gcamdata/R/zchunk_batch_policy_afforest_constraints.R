# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_afforest_constraints_xml
#'
#' Construct XML data structure for \code{policy_afforest_constraints.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_afforest_constraints.xml}.
module_policy_afforest_constraints_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L3231.affor_mngd_nodes",
             "L3231.affor_unmngd_nodes",
             "L3231.affor_constraint"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "policy_afforest_constraints.xml"))
  } else if(command == driver.MAKE) {
    
    all_data <- list(...)[[1]]
    
    # Load required inputs
    L3231.affor_mngd_nodes <- get_data(all_data, "L3231.affor_mngd_nodes")
    L3231.affor_unmngd_nodes <- get_data(all_data, "L3231.affor_unmngd_nodes")
    L3231.affor_constraint <- get_data(all_data, "L3231.affor_constraint")
    # ===================================================
    
    # Produce outputs
    create_xml("policy_afforest_constraints.xml") %>%
      add_xml_data(L3231.affor_constraint, "PortfolioStdConstraint") %>%
      add_xml_data(L3231.affor_unmngd_nodes, "AfforestConstraintUnmgd") %>%
      add_xml_data(L3231.affor_mngd_nodes, "AfforestConstraintMgd") %>%
      add_rename_landnode_xml() %>%
      add_precursors("L3231.affor_mngd_nodes",
                     "L3231.affor_unmngd_nodes",
                     "L3231.affor_constraint") ->
      policy_afforest_constraints.xml
    
    return_data(policy_afforest_constraints.xml)
  } else {
    stop("Unknown command")
  }
}
