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
  all_xml_names <- get_xml_names("policy/A_Affor_Constraints.csv", "policy_afforest_constraints.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(c("L3231.affor_mngd_nodes",
             "L3231.affor_unmngd_nodes",
             "L3231.affor_constraint"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
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

    for (xml_name in all_xml_names){
      L3231.affor_constraint_tmp <- L3231.affor_constraint %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L3231.affor_unmngd_nodes_tmp <- L3231.affor_unmngd_nodes %>%
        semi_join(L3231.affor_constraint_tmp, by = "region")

      L3231.affor_mngd_nodes_tmp <- L3231.affor_mngd_nodes  %>%
        semi_join(L3231.affor_constraint_tmp, by = "region")

      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(L3231.affor_constraint_tmp, "PortfolioStdConstraint") %>%
               add_xml_data(L3231.affor_unmngd_nodes_tmp, "AfforestConstraintUnmgd") %>%
               add_xml_data(L3231.affor_mngd_nodes_tmp, "AfforestConstraintMgd") %>%
               add_rename_landnode_xml() %>%
               add_precursors("L3231.affor_mngd_nodes",
                              "L3231.affor_unmngd_nodes",
                              "L3231.affor_constraint")
      )
    }

    # Need this for loop because having issues with lapply(all_xml_names, get)
    list_of_xmls <- list()
    for(xml_name in all_xml_names){
      list_of_xmls[[xml_name]] <- get(xml_name)
    }
    return_multiple_xmls(list_of_xmls, all_xml_names)
  } else {
    stop("Unknown command")
  }
}
