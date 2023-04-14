# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_CCap_xml
#'
#' Construct XML data structure for \code{CCap.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_CCap.xml}.
module_policy_CCap_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L3221.CCap_constraint",
             "L3221.CCap_link_regions",
             "L3221.CCap_tech",
             "L3221.CCap_tranTech",
             "L3221.CCap_resource"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "policy_CCap.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L3221.CCap_constraint <- get_data(all_data, "L3221.CCap_constraint")
    L3221.CCap_link_regions <- get_data(all_data, "L3221.CCap_link_regions")
    L3221.CCap_tech <- get_data(all_data, "L3221.CCap_tech")
    L3221.CCap_tranTech <- get_data(all_data, "L3221.CCap_tranTech")
    L3221.CCap_resource <- get_data(all_data, "L3221.CCap_resource")
    # ===================================================
    # Need to split L3221.CCap_constraint into years with fillout and years without
    L3221.CCap_constraint_fillout <- L3221.CCap_constraint %>%
      filter(year.fillout == constraint.year)

    L3221.CCap_constraint_noFillout <- L3221.CCap_constraint %>%
      filter(is.na(year.fillout) | year.fillout != constraint.year) %>%
      select(-year.fillout)

    # Produce outputs
    create_xml("policy_CCap.xml") %>%
      add_xml_data(L3221.CCap_constraint_noFillout, "GHGConstr") %>%
      add_xml_data(L3221.CCap_constraint_fillout, "GHGConstrFillout") %>%
      add_xml_data(L3221.CCap_link_regions, "GHGConstrMkt") %>%
      add_xml_data(L3221.CCap_tech, "StubTechCO2") %>%
      add_xml_data(L3221.CCap_tranTech, "StubTranTechCO2") %>%
      add_xml_data(L3221.CCap_resource, "ResTechCO2") %>%
      add_precursors("L3221.CCap_constraint",
                     "L3221.CCap_link_regions",
                     "L3221.CCap_tech",
                     "L3221.CCap_tranTech",
                     "L3221.CCap_resource") ->
      policy_CCap.xml

    return_data(policy_CCap.xml)
  } else {
    stop("Unknown command")
  }
}
