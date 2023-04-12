# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L322.biofuel_targets
#'
#' Produce biofuel/biodiesel targets and markets by region
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L322.biofuel_targets}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L322.biofuel_targets <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_Biofuel_Targets",
             FILE = "policy/A_Biofuel_SecOut"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L322.policy_port_stnd",
             "L322.biofuel_policy_coefs",
             "L322.biofuel_secout"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_Biofuel_Targets <- get_data(all_data, "policy/A_Biofuel_Targets")
    A_Biofuel_SecOut <- get_data(all_data, "policy/A_Biofuel_SecOut")

    # Extend targets to all desired years
    # Filter out NA years and interpolate between non-NA years
    L322.biofuel_targets_NA <- A_Biofuel_Targets %>%
      gather_years() %>%
      na.omit() %>%
      group_by(region, market, policy.portfolio.standard) %>%
      # Interpolates between min and max years for each region/policy combo
      complete(nesting(region, market, policy.portfolio.standard), year = seq(min(year), max(year), 5))

    # Separate out groups with only 1 value since they get turned into NAs with approx_fun
    L322.biofuel_targets_n1 <- L322.biofuel_targets_NA %>%
      filter(dplyr::n() == 1) %>%
      ungroup

    L322.biofuel_targets <- L322.biofuel_targets_NA %>%
      filter(dplyr::n() > 1)%>%
      mutate(value = approx_fun(year, value)) %>%
      ungroup %>%
      bind_rows(L322.biofuel_targets_n1) %>%
      arrange(region, year)

    # Create policy portfolio standard tables
    L322.policy_port_stnd <- L322.biofuel_targets %>%
      mutate(policyType = "RES",
             constraint = 1) %>%
      select(-value)

    # Create policy coefficient tables
    L322.biofuel_policy_coefs <- L322.biofuel_targets %>%
      mutate(supplysector = "refined liquids enduse",
             subsector = "refined liquids enduse",
             technology = "refined liquids enduse") %>%
      select(-market) %>%
      rename(minicam.energy.input = policy.portfolio.standard,
             coefficient = value)

    # Create secondary output tables - interpolate between years
    L322.biofuel_secout <- A_Biofuel_SecOut %>%
      gather_years(value_col = "res.secondary.output") %>%
      group_by(region, supplysector, subsector, technology) %>%
      # Interpolates between min and max years for each region/output combo
      complete(nesting(region, supplysector, subsector, technology),
               year = seq(min(year), max(year), 5)) %>%
      mutate(res.secondary.output =if_else(is.na(res.secondary.output),
                                           res.secondary.output[year == max(year)],
                                           res.secondary.output)) %>%
      ungroup %>%
      mutate(output.ratio = 1)


    # Produce outputs
    L322.policy_port_stnd %>%
      add_title("Biofuel policy names to set RES markets", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_Biofuel_Targets") ->
      L322.policy_port_stnd

    L322.biofuel_policy_coefs %>%
      add_title("Biofuel policy coefficients for RES markets", overwrite = T) %>%
      add_units("Proportion of refined liquids that must come from biofuels") %>%
      add_precursors("policy/A_Biofuel_Targets") ->
      L322.biofuel_policy_coefs

    L322.biofuel_secout %>%
      add_title("Biofuel secondary output for RES markets", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_Biofuel_SecOut") ->
      L322.biofuel_secout

    return_data(L322.policy_port_stnd, L322.biofuel_policy_coefs, L322.biofuel_secout)
  } else {
    stop("Unknown command")
  }
}

