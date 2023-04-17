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
             "L301.input_tax",
             FILE = "policy/A_Policy_XML_Names"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    # output files are unknown until we read in A_Policy_XML_Name
    all_xml_names <- suppressMessages(readr::read_csv("inst/extdata/policy/A_Policy_XML_Names.csv", comment = "#"))
    all_xml_names <- union(unique(all_xml_names$xml), "policy_ceilings_floors.xml")
    names(all_xml_names) <- rep("XML", length(all_xml_names))
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L301.policy_RES_coefs <- get_data(all_data, "L301.policy_RES_coefs")
    L301.RES_secout <- get_data(all_data, "L301.RES_secout")
    L301.input_tax <- get_data(all_data, "L301.input_tax")
    L301.policy_port_stnd <- get_data(all_data, "L301.policy_port_stnd")
    A_Policy_XML_Names <- get_data(all_data, "policy/A_Policy_XML_Names")

    # Match XML names in A_Policy_XML_Names to policies
    # If no xml listed for given region/market/policy, assign to policy_ceilings_floors.xml
    L301.policy_port_stnd_xml <- L301.policy_port_stnd %>%
      left_join(A_Policy_XML_Names, by = c("policy.portfolio.standard", "market")) %>%
      replace_na(list(xml = "policy_ceilings_floors.xml"))

    all_xml_names <- union(unique(A_Policy_XML_Names$xml), "policy_ceilings_floors.xml")
    # ===================================================

    for (xml_name in all_xml_names){
      L301.policy_port_stnd_tmp <- L301.policy_port_stnd_xml %>%
        filter(xml == xml_name) %>%
        select(-xml)

      policy_rgn_tmp <- L301.policy_port_stnd_tmp %>%
        distinct(region, policyType)

      if ("RES" %in% policy_rgn_tmp$policyType){
        L301.policy_RES_coefs_tmp <- L301.policy_RES_coefs %>%
          semi_join(policy_rgn_tmp, by = c("region", "policyType"))

        L301.RES_secout_tmp <- L301.RES_secout %>%
          semi_join(select(policy_rgn_tmp, region), by = c("region"))
      } else {
        # If policy type not included, still need to include the tables
        # So just make them empty
        L301.policy_RES_coefs_tmp <- L301.policy_RES_coefs[0,]
        L301.RES_secout_tmp <- L301.RES_secout[0,]
      }

      if ("tax" %in% policy_rgn_tmp$policyType){
        L301.input_tax_tmp <- L301.input_tax %>%
          semi_join(select(policy_rgn_tmp, region), by = c("region"))
      } else {
        # If policy type not included, still need to include the tables
        # So just make them empty
        L301.input_tax_tmp <- L301.input_tax[0,]
      }

      # Produce output
      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(L301.policy_port_stnd_tmp, "PortfolioStdConstraint") %>%
               add_xml_data(L301.policy_RES_coefs_tmp, "StubTechCoefIndUrb") %>%
               add_xml_data(L301.RES_secout_tmp, "TechResSecOut") %>%
               add_xml_data(L301.input_tax_tmp, "TechInputTax") %>%
               add_precursors("L301.policy_port_stnd",
                              "L301.policy_RES_coefs",
                              "L301.RES_secout",
                              "L301.input_tax")
             )

    }


    list_of_outputs <- lapply(all_xml_names, get)
    names(list_of_outputs) <- all_xml_names
    outlist <- sapply(list_of_outputs, return_data)
    names(outlist) <- all_xml_names
    return(outlist)
  } else {
    stop("Unknown command")
  }
}
