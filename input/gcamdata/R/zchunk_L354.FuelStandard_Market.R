# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L354.FuelStandards_Market
#'
#' Produce fuel standards coefficients
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L354.FuelStandards_coef},\code{L354.FuelStandards_secout},\code{L354.FuelStandards_policy}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L354.FuelStandards_Market <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_FuelStandards_Market",
             "L254.StubTranTechCoef",
             "L254.StubTranTechLoadFactor",
             FILE = "emissions/A_PrimaryFuelCCoef"
    ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L354.FuelStandards_coef",
             "L354.FuelStandards_secout",
             "L354.FuelStandards_policy"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_FuelStandards_Market <- get_data(all_data, "policy/A_FuelStandards_Market")
    A_PrimaryFuelCCoef <- get_data(all_data, "emissions/A_PrimaryFuelCCoef")
    L254.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef")
    L254.StubTranTechLoadFactor <- get_data(all_data, "L254.StubTranTechLoadFactor") %>%
      distinct(region, supplysector, tranSubsector, year, loadFactor, sce)

    # Need this so that coefficient will end up as vkm
    BTU_per_EJ <- 1 / (CONV_KBTU_EJ/1000)

    # Convert to long and interpolate between model years
    A_FuelStandards_Market <- A_FuelStandards_Market %>%
      gather_years(value_col = "coefficient") %>%
      filter(!is.na(coefficient)) %>%
      group_by(xml, policy_name, market, SSP_sce, region, supplysector, tranSubsector, Units) %>%
      complete(nesting(xml, policy_name, market, SSP_sce, region, supplysector, tranSubsector, Units),
               year = seq(min(year), max(year), 5)) %>%
      mutate(coefficient = approx_fun(year, coefficient)) %>%
      ungroup

    L354.FuelStandards_Market <- A_FuelStandards_Market %>%
      # Join in default GCAM coefficients and load factors and calculate coefficient and sec output for fuel market
      left_join(L254.StubTranTechCoef,
                               by = c("region", "supplysector", "tranSubsector",
                                      "year","SSP_sce" = "sce")) %>%
      # A few places where there are slight differences in loadfactor by tech (probably an error)
      # so here just keep first since the differences are so small
      left_join_keep_first_only(L254.StubTranTechLoadFactor,
                by = c("region", "supplysector", "tranSubsector",
                       "year","SSP_sce" = "sce")) %>%
      left_join(A_PrimaryFuelCCoef, by = c("minicam.energy.input" = "PrimaryFuelCO2Coef.name")) %>%
      # Every policy needs different name because we want this to apply to new cars only
      mutate(gCO2_per_vkm = coefficient.y * CONV_BTU_KJ / 1e3 * PrimaryFuelCO2Coef * 44 / 12,
             coefficient = if_else(is.na(gCO2_per_vkm), 0, BTU_per_EJ * gCO2_per_vkm * loadFactor / coefficient.x / 1e6),
             output.ratio = 1,
             policyType = "RES",
             # Every policy needs different name because we want this to apply to new cars only
             policy_name = paste0(policy_name, year))

    # Produce outputs
    L354.FuelStandards_Market %>%
      select(xml, region, supplysector, tranSubsector, stub.technology, year,
             minicam.energy.input = policy_name, coefficient) %>%
      add_title("Policy fuel coefficients", overwrite = T) %>%
      add_units("BTU/EJ") %>%
      add_precursors("policy/A_FuelStandards_Market",
                     "L254.StubTranTechCoef",
                     "emissions/A_PrimaryFuelCCoef") ->
      L354.FuelStandards_coef

    L354.FuelStandards_Market %>%
      select(xml, region, supplysector, tranSubsector, stub.technology, year,
             res.secondary.output = policy_name, output.ratio) %>%
      add_title("Policy fuel secondary output", overwrite = T) %>%
      add_units("million vkm") %>%
      add_precursors("policy/A_FuelStandards_Market",
                     "L254.StubTranTechCoef",
                     "emissions/A_PrimaryFuelCCoef") ->
      L354.FuelStandards_secout

    L354.FuelStandards_Market %>%
      select(xml, region, policy.portfolio.standard = policy_name,
            market, policyType, year) %>%
      mutate(constraint = 1) %>%
      distinct() %>%
      add_title("Policy fuel market", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_FuelStandards_Market",
                     "L254.StubTranTechCoef",
                     "emissions/A_PrimaryFuelCCoef") ->
    L354.FuelStandards_policy

    return_data(L354.FuelStandards_coef, L354.FuelStandards_secout, L354.FuelStandards_policy)
  } else {
    stop("Unknown command")
  }
}

