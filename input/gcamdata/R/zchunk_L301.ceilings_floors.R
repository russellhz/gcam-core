# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L301.ceilings_floors
#'
#' Produce ceilings/floors and markets by region
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L301.policy_port_stnd}, \code{L301.policy_RES_coefs},
#' \code{L301.RES_secout}, \code{L301.input_tax}, \code{L301.input_subsidy}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L301.ceilings_floors <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_Policy_Constraints",
             FILE = "policy/A_Policy_Constraints_Techs",
             FILE = "policy/A_Policy_RES_Coefs",
             FILE = "policy/A_Policy_RES_SecOut",
             "L226.StubTechCoef_elecownuse",
             "L226.StubTechCoef_electd"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L301.policy_port_stnd",
             "L301.policy_RES_coefs",
             "L301.RES_secout",
             "L301.input_tax",
             "L301.input_subsidy",
             "L301.XML_policy_map"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_Policy_Constraints <- get_data(all_data, "policy/A_Policy_Constraints")
    A_Policy_Constraints_Techs <- get_data(all_data, "policy/A_Policy_Constraints_Techs")
    A_Policy_RES_Coefs <- get_data(all_data, "policy/A_Policy_RES_Coefs")
    A_Policy_RES_SecOut <- get_data(all_data, "policy/A_Policy_RES_SecOut")
    L226.StubTechCoef_elecownuse <- get_data(all_data, "L226.StubTechCoef_elecownuse")
    L226.StubTechCoef_electd <- get_data(all_data, "L226.StubTechCoef_electd")

    # 1. Extend targets to all desired years
    # Filter out NA years and interpolate between non-NA years
    L301.ceilings_floors_NA <- bind_rows(A_Policy_Constraints, A_Policy_RES_Coefs) %>%
      gather_years(value_col = "constraint") %>%
      filter(!is.na(constraint)) %>%
      # Leaving grouped on purpose here
      group_by(region, market, policy.portfolio.standard, policyType,
               supplysector, subsector, stub.technology) %>%
      # Interpolates between min and max years for each region/policy combo
      complete(nesting(xml, region, market, policy.portfolio.standard, policyType,
                       supplysector, subsector, stub.technology),
               year = seq(min(year), max(year), 5))

    # Separate out groups with only 1 value since they get turned into NAs with approx_fun
    L301.ceilings_floors_n1 <- L301.ceilings_floors_NA %>%
      filter(dplyr::n() == 1) %>%
      ungroup

    L301.ceilings_floors <- L301.ceilings_floors_NA %>%
      filter(dplyr::n() > 1)%>%
      mutate(constraint = approx_fun(year, constraint)) %>%
      ungroup %>%
      bind_rows(L301.ceilings_floors_n1) %>%
      arrange(region, policyType, year) %>%
      select(-xml)

    # 2. Create policy portfolio standard tables
    L301.policy_port_stnd <- L301.ceilings_floors %>%
      mutate(constraint = if_else(policyType == "RES",
                                  1,
                                  constraint)) %>%
      select(LEVEL2_DATA_NAMES[["PortfolioStdConstraint"]]) %>%
      distinct()

    # 3. Create RES coefficient tables
    L301.policy_RES_coefs <- L301.ceilings_floors %>%
      filter(policyType  == "RES") %>%
      select(-market) %>%
      rename(minicam.energy.input = policy.portfolio.standard,
             coefficient = constraint)

    # 4. Create secondary output tables - interpolate between years
    L301.RES_secout <-  A_Policy_RES_SecOut %>%
      tidyr::pivot_longer(start.year:end.year, names_to = "drop", values_to = "year") %>%
      select(-drop) %>%
      distinct() %>%
      group_by(region, supplysector, subsector, stub.technology,
               res.secondary.output, output.ratio, calculate_elec_losses) %>%
      # Interpolates between min and max years for each region/output combo
      complete(nesting(region, supplysector, subsector, stub.technology,
                       res.secondary.output, output.ratio, calculate_elec_losses),
               year = seq(min(year), max(year), 5)) %>%
      ungroup %>%
      arrange(year)

    # Calculate electricity losses if specified
    secout_elec_losses <- L301.RES_secout %>%
      filter(calculate_elec_losses == 1)

    L301.RES_secout <- L301.RES_secout %>%
      filter(is.na(calculate_elec_losses)) %>%
      select(-calculate_elec_losses)

    if (nrow(secout_elec_losses) > 0){
      # This works because each region has just one coefficient per year
      # If that changes, will need to change
      # Just in case, we confirm first
      elecownuse_coefs <- distinct(L226.StubTechCoef_elecownuse, region, year, coefficient)
      stopifnot(dplyr::n_groups(group_by(elecownuse_coefs, region, year)) == nrow(elecownuse_coefs))

      electd_coefs <- distinct(L226.StubTechCoef_electd, region, year, coefficient)
      stopifnot(dplyr::n_groups(group_by(electd_coefs, region, year)) == nrow(electd_coefs))

      elec_losses <- secout_elec_losses %>%
        left_join_error_no_match(elecownuse_coefs,
                  by = c("region", "year")) %>%
        left_join_error_no_match(electd_coefs,
                  by = c("region", "year")) %>%
        mutate(output.ratio = 1 / (coefficient.x * coefficient.y)) %>%
        select(-coefficient.x, -coefficient.y, -calculate_elec_losses)

      L301.RES_secout <- L301.RES_secout %>%
       bind_rows(elec_losses)
    }



    # 5. Create input tax tables - interpolate between years
    L301.input_tax <- A_Policy_Constraints_Techs %>%
      filter(!is.na(input.tax)) %>%
      select(-input.subsidy) %>%
      tidyr::pivot_longer(start.year:end.year, names_to = "drop", values_to = "year") %>%
      select(-drop) %>%
      distinct() %>%
      group_by(region, supplysector, subsector, stub.technology, input.tax) %>%
      # Interpolates between min and max years for each region/output combo
      complete(nesting(region, supplysector, subsector, stub.technology, input.tax),
               year = seq(min(year), max(year), 5)) %>%
      ungroup %>%
      arrange(year)

    # 6. Create input subsidy tables - interpolate between years
    L301.input_subsidy <- A_Policy_Constraints_Techs %>%
      filter(!is.na(input.subsidy)) %>%
      select(-input.tax) %>%
      tidyr::pivot_longer(start.year:end.year, names_to = "drop", values_to = "year") %>%
      select(-drop) %>%
      distinct() %>%
      group_by(region, supplysector, subsector, stub.technology, input.subsidy) %>%
      # Interpolates between min and max years for each region/output combo
      complete(nesting(region, supplysector, subsector, stub.technology, input.subsidy),
               year = seq(min(year), max(year), 5)) %>%
      ungroup %>%
      arrange(year)


    # 7. Make mapping of policies to xml file names
    L301.XML_policy_map <- distinct(A_Policy_Constraints, xml, policy.portfolio.standard, market) %>%
      bind_rows(distinct(A_Policy_RES_Coefs, xml, policy.portfolio.standard, market))

    # Produce outputs
    L301.policy_port_stnd %>%
      add_title("Policy names and constraints", overwrite = T) %>%
      add_units("EJ (for taxes) or NA (for RES)") %>%
      add_precursors("policy/A_Policy_Constraints",
                     "policy/A_Policy_RES_Coefs") ->
      L301.policy_port_stnd

    L301.policy_RES_coefs %>%
      add_title("Coefficients for RES markets", overwrite = T) %>%
      add_units("Proportion of supplysector/subsector/technology") %>%
      add_precursors("policy/A_Policy_Constraints",
                     "policy/A_Policy_RES_Coefs") ->
      L301.policy_RES_coefs

    L301.RES_secout %>%
      add_title("Secondary output for RES markets", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_Policy_RES_SecOut") ->
      L301.RES_secout

    L301.input_tax %>%
      add_title("Technologies to apply constraint to", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_Policy_RES_SecOut") ->
      L301.input_tax

    L301.input_subsidy %>%
      add_title("Technologies to apply constraint to", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_Policy_RES_SecOut") ->
      L301.input_subsidy

    L301.XML_policy_map %>%
      add_title("Mapping of policy names to xml", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_Policy_Constraints",
                     "policy/A_Policy_RES_Coefs") ->
      L301.XML_policy_map

    return_data(L301.policy_port_stnd,
                L301.policy_RES_coefs,
                L301.RES_secout,
                L301.input_tax,
                L301.input_subsidy,
                L301.XML_policy_map)
  } else {
    stop("Unknown command")
  }
}

