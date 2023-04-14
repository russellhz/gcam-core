# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L3221.CCap
#'
#' Produce custom carbon cap targets and markets
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L3221.CCap_constraint}, \code{L3221.CCap_no_constraint}, \code{L3221.CCap_tech}, \code{L3221.CCap_tranTech}, \code{L3221.CCap_resource}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L3221.CCap <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_CCap_Constraint",
             FILE = "policy/A_CCap_Region",
             FILE = "policy/A_CCap_Sector",
             FILE = "policy/A_CCap_Resource",
             "L210.ResTechCoef",
             "L221.StubTech_en",
             "L222.StubTech_en",
             "L224.StubTech_heat",
             "L2233.StubTech_elec_cool",
             "L226.StubTech_en",
             "L232.StubTech_ind",
             "L2321.StubTech_cement",
             "L2322.StubTech_Fert",
             "L2323.StubTech_iron_steel",
             "L2324.StubTech_Off_road",
             "L2325.StubTech_chemical",
             "L2326.StubTech_aluminum",
             "L244.StubTech_bld",
             "L254.StubTranTech"
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L3221.CCap_constraint",
             "L3221.CCap_no_constraint",
             "L3221.CCap_tech",
             "L3221.CCap_tranTech",
             "L3221.CCap_resource"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_CCap_Constraint <- get_data(all_data, "policy/A_CCap_Constraint")
    A_CCap_Region <- get_data(all_data, "policy/A_CCap_Region")
    A_CCap_Sector <- get_data(all_data, "policy/A_CCap_Sector")
    A_CCap_Resource <- get_data(all_data, "policy/A_CCap_Resource")
    L3221.StubTech_All <- bind_rows(get_data(all_data, "L221.StubTech_en"),
                                    get_data(all_data, "L222.StubTech_en"),
                                    get_data(all_data, "L2233.StubTech_elec_cool"),
                                    get_data(all_data, "L224.StubTech_heat"),
                                    get_data(all_data, "L226.StubTech_en"),
                                    get_data(all_data, "L232.StubTech_ind"),
                                    get_data(all_data, "L2321.StubTech_cement"),
                                    get_data(all_data, "L2322.StubTech_Fert"),
                                    get_data(all_data, "L2323.StubTech_iron_steel"),
                                    get_data(all_data, "L2324.StubTech_Off_road"),
                                    get_data(all_data, "L2325.StubTech_chemical"),
                                    get_data(all_data, "L2326.StubTech_aluminum"),
                                    get_data(all_data, "L244.StubTech_bld")
                                    )
    L254.StubTranTech <- get_data(all_data, "L254.StubTranTech") %>%
                                      filter(sce == "CORE") %>%
                                      select(-sce)
    L210.ResTech <- get_data(all_data, "L210.ResTechCoef") %>%
      distinct(region, resource, reserve.subresource, resource.reserve.technology)
    # Write constraint to correct regions
    # When a constraint applies to more than one region, we don't need to write the
    # constraint to all regions, can just pass the ghg policy forward
    constraint_regions <- A_CCap_Region %>%
      group_by(market, ghgpolicy) %>%
      filter(row_number() == 1) %>%
      ungroup

    L3221.CCap_constraint <- A_CCap_Constraint %>%
      gather_years() %>%
      filter(!is.na(value)) %>%
      left_join_error_no_match(constraint_regions, by = c("market", "ghgpolicy")) %>%
      rename(constraint.year = year, constraint = value)

    L3221.CCap_no_constraint <- A_CCap_Region %>%
      anti_join(L3221.CCap_constraint, by = c("region", "market", "ghgpolicy"))

    # Add custom CO2 market to the associated regions/sectors
    # Need to add all techs for the given regions/sectors
    # First for non-transport techs, then for transport techs, then for resources
    L3221.CCap_tech <- A_CCap_Sector %>%
      filter(is.na(tranSubsector)) %>%
      select(-tranSubsector) %>%
      left_join(A_CCap_Region, by = c("market", "ghgpolicy")) %>%
      # Remove district heat from EU-15
      filter(!(region == "EU-15" & supplysector == "district heat")) %>%
      left_join(L3221.StubTech_All, by = c("supplysector", "region")) %>%
      rename(CO2 = ghgpolicy) %>%
      select(-market) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS))

    # Check that there are no NAs
    stopifnot(!any(is.na(L3221.CCap_tech)))

    L3221.CCap_tranTech <- A_CCap_Sector %>%
      filter(!is.na(tranSubsector)) %>%
      left_join(A_CCap_Region, by = c("market", "ghgpolicy")) %>%
      left_join(L254.StubTranTech, by = c("supplysector", "region", "tranSubsector")) %>%
      rename(CO2 = ghgpolicy) %>%
      select(-market) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS))

    # Check that there are no NAs
    stopifnot(!any(is.na(L3221.CCap_tranTech)))

    L3221.CCap_resource <- A_CCap_Resource %>%
      left_join(A_CCap_Region, by = c("market", "ghgpolicy")) %>%
      left_join(L210.ResTech, by = c("resource", "reserve.subresource", "region")) %>%
      rename(CO2 = ghgpolicy) %>%
      select(-market) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS))

    # Check that there are no NAs
    stopifnot(!any(is.na(L3221.CCap_resource)))

    # Produce outputs
    L3221.CCap_constraint %>%
      add_title("Custom carbon constraints", overwrite = T) %>%
      add_units("MTC") %>%
      add_precursors("policy/A_CCap_Constraint",
                     "policy/A_CCap_Region") ->
      L3221.CCap_constraint

    L3221.CCap_no_constraint %>%
      add_title("Linked regions to carbon constraints", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_CCap_Constraint",
                     "policy/A_CCap_Region") ->
      L3221.CCap_no_constraint

    L3221.CCap_tech %>%
      add_title("Technology mapping to custom ghg policy", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_CCap_Region",
                     "policy/A_CCap_Sector",
                     "L221.StubTech_en",
                     "L222.StubTech_en",
                     "L224.StubTech_heat",
                     "L2233.StubTech_elec_cool",
                     "L226.StubTech_en",
                     "L232.StubTech_ind",
                     "L2321.StubTech_cement",
                     "L2322.StubTech_Fert",
                     "L2323.StubTech_iron_steel",
                     "L2324.StubTech_Off_road",
                     "L2325.StubTech_chemical",
                     "L2326.StubTech_aluminum",
                     "L244.StubTech_bld") ->
      L3221.CCap_tech

    L3221.CCap_tranTech %>%
      add_title("Transport technology mapping to custom ghg policy", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_CCap_Region",
                     "policy/A_CCap_Sector",
                     "L254.StubTranTech") ->
      L3221.CCap_tranTech

    L3221.CCap_resource %>%
      add_title("Resource technology mapping to custom ghg policy", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_CCap_Region",
                     "policy/A_CCap_Resource",
                     "L210.ResTechCoef") ->
      L3221.CCap_resource

    return_data(L3221.CCap_constraint, L3221.CCap_no_constraint,
                L3221.CCap_tech, L3221.CCap_tranTech, L3221.CCap_resource)
  } else {
    stop("Unknown command")
  }
}

