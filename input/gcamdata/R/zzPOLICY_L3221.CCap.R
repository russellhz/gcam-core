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
#' the generated outputs: \code{L3221.CCap_constraint}, \code{L3221.CCap_link_regions}, \code{L3221.CCap_tech}, \code{L3221.CCap_tranTech}, \code{L3221.CCap_resource}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L3221.CCap <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_CCap_Constraint",
             FILE = "policy/A_CCap_Sector",
             FILE = "policy/A_CCap_Resource",
             FILE = "policy/A_CTax_Link",
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
             "L254.StubTranTech",
             "L201.GDP_Scen",
             FILE = "policy/A_CO2ByTech"
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L3221.CCap_constraint",
             "L3221.CCap_link_regions",
             "L3221.CCap_tech",
             "L3221.CCap_tranTech",
             "L3221.CCap_resource",
             "L3221.CCap_GHG_Link"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_CCap_Constraint <- get_data(all_data, "policy/A_CCap_Constraint")
    A_CCap_Sector <- get_data(all_data, "policy/A_CCap_Sector")
    A_CCap_Resource <- get_data(all_data, "policy/A_CCap_Resource")
    A_CTax_Link <- get_data(all_data, "policy/A_CTax_Link")

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

    L201.GDP_Scen <- get_data(all_data, "L201.GDP_Scen")
    A_CO2ByTech <- get_data(all_data, "policy/A_CO2ByTech") %>%
      gather_years()

    # 1. Write constraint to correct regions
    # When a constraint applies to more than one region, we don't need to write the
    # constraint to all regions, can just pass the ghg policy forward to the "linked" regions
    L3221.CCap_constraint <- A_CCap_Constraint %>%
      gather_years() %>%
      # Assumes that linked regions have no constraint listed
      filter(!is.na(value)) %>%
      rename(constraint.year = year, constraint = value)

    L3221.CCap_link_regions <- A_CCap_Constraint %>%
      gather_years() %>%
      group_by(region, market, ghgpolicy) %>%
      # Assumes that linked regions have no constraint listed
      filter(all(is.na(value))) %>%
      ungroup %>%
      distinct(xml, region, market, ghgpolicy)

    # 2. Add custom CO2 market to stub technologies
    # Note that we remove all biomass technologies since they shouldn't contribute to
    # CO2 constraints in most cases
    # For technologies, we need all regions
    tech_all_regions <- A_CCap_Constraint %>%
      distinct(market, region, ghgpolicy)

    # Need to add all techs for the given regions/sectors
    # First for non-transport techs, then for transport techs, then for resources
    L3221.CCap_tech <- A_CCap_Sector %>%
      filter(is.na(tranSubsector)) %>%
      select(-tranSubsector) %>%
      left_join(tech_all_regions, by = c("market", "ghgpolicy")) %>%
      left_join(L3221.StubTech_All, by = c("supplysector", "region")) %>%
      # Remove bio techs
      filter(!grepl("bio", subsector, ignore.case = T),
             !grepl("bio", stub.technology, ignore.case = T)) %>%
      rename(CO2 = ghgpolicy) %>%
      select(-market) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS))

    L3221.CCap_tech_NAs <- L3221.CCap_tech %>%
      filter(is.na(stub.technology)) %>%
      distinct(CO2, region, supplysector)

    # If there are NAs, we need to remove them, but here we print a message with any NA sectors
    if (nrow(L3221.CCap_tech_NAs) > 0){
      print("Constraint removed for the following sectors:")
      for(i in 1:nrow(L3221.CCap_tech_NAs)){
        print(paste(L3221.CCap_tech_NAs[i,], collapse = "---"))
      }
    }

    L3221.CCap_tech <- filter(L3221.CCap_tech, !is.na(stub.technology))

    # Shouldn't have any NAs left
    stopifnot(!any(is.na(L3221.CCap_tech)))

    # 3. Add custom CO2 market to transportation technologies
    L3221.CCap_tranTech <- A_CCap_Sector %>%
      filter(!is.na(tranSubsector)) %>%
      left_join(tech_all_regions, by = c("market", "ghgpolicy")) %>%
      left_join(L254.StubTranTech, by = c("supplysector", "region", "tranSubsector")) %>%
      rename(CO2 = ghgpolicy) %>%
      select(-market) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS))

    # Check that there are no NAs
    stopifnot(!any(is.na(L3221.CCap_tranTech)))

    # 4. Add custom CO2 market to resource technologies
    L3221.CCap_resource <- A_CCap_Resource %>%
      left_join(tech_all_regions, by = c("market", "ghgpolicy")) %>%
      left_join(L210.ResTech, by = c("resource", "reserve.subresource", "region")) %>%
      rename(CO2 = ghgpolicy) %>%
      select(-market) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS))

    # Check that there are no NAs
    stopifnot(!any(is.na(L3221.CCap_resource)))

    # 5. Adjust any GDPIntensity targets
    if (any(!is.na(L3221.CCap_constraint$GDPIntensity_BaseYear))){
      GDP_Intensity_targets <- L3221.CCap_constraint %>%
        filter(!is.na(GDPIntensity_BaseYear))

      # First calculate GDP series - we have baseGDP, growth rate in perCapitaGDP and population
      L3221.GDP <- L201.GDP_Scen %>%
        filter(scenario == paste0("g", socioeconomics.BASE_GDP_SCENARIO))

      # Next get the energy technologies that we want to constraint
      L3221.emissions_techs <- bind_rows(L3221.CCap_tech,
                                      rename(L3221.CCap_tranTech, subsector = tranSubsector),
                                      rename(L3221.CCap_resource, supplysector = resource,
                                             subsector = reserve.subresource,
                                             stub.technology = resource.reserve.technology)) %>%
        semi_join(GDP_Intensity_targets, by = c("region", "CO2" = "ghgpolicy", "year" = "constraint.year")) %>%
        left_join_error_no_match(select(GDP_Intensity_targets, region, CO2 = ghgpolicy,
                                        GDPIntensity_BaseYear, constraint, year = constraint.year),
                                 by = c("region", "CO2", "year"))

      # Calculate the CO2 emissions of the techs in the base year
      L3221.baseEmissions <- L3221.emissions_techs %>%
        select(-year, -constraint) %>%
        distinct() %>%
        left_join(A_CO2ByTech, by = c("region", "supplysector" = "sector", "subsector",
                                                         "stub.technology" = "technology", "GDPIntensity_BaseYear" = "year")) %>%
        group_by(region, CO2, GDPIntensity_BaseYear) %>%
        summarise(value = sum(value, na.rm = T)) %>%
        ungroup

      # Add in GDP and calculate base GDP Intensity
      L3221.baseIntensity <- L3221.baseEmissions %>%
        left_join_error_no_match(L3221.GDP, by = c("region", "GDPIntensity_BaseYear" = "year")) %>%
        mutate(GDP_intensity = value / GDP) %>%
        select(region, CO2, GDPIntensity_BaseYear, GDP_intensity)

      # Future GDP intensities
      L3221.futureIntensity <- GDP_Intensity_targets %>%
        left_join_error_no_match(L3221.baseIntensity,
                                 by = c("region", "ghgpolicy" = "CO2", "GDPIntensity_BaseYear")) %>%
        mutate(GDP_intensity = GDP_intensity * (100 + constraint) / 100)

      # Calculate future energy constraints based on GDP intensities
      L3221.futureEmissions <- L3221.futureIntensity %>%
        left_join_error_no_match(L3221.GDP, by = c("region", "constraint.year" = "year")) %>%
        mutate(constraint = GDP_intensity * GDP) %>%
        select(xml, region, market, ghgpolicy, year.fillout, constraint.year, constraint)

      L3221.CCap_constraint <- L3221.CCap_constraint %>%
        filter(is.na(GDPIntensity_BaseYear)) %>%
        bind_rows(L3221.futureEmissions)


    }

    # 6. Get ghg link for each
    L3221.CCap_GHG_Link <- A_CCap_Constraint %>%
      distinct(link.type, market, region, ghgpolicy) %>%
      filter(!is.na(link.type)) %>%
      left_join(A_CTax_Link, by = "link.type") %>%
      rename(linked.policy = ghgpolicy) %>%
      select(-link.type)

    # Produce outputs
    L3221.CCap_constraint %>%
      select(-GDPIntensity_BaseYear, -link.type) %>%
      add_title("Custom carbon constraints", overwrite = T) %>%
      add_units("MTC") %>%
      add_precursors("policy/A_CCap_Constraint",
                     "policy/A_CCap_Region") ->
      L3221.CCap_constraint

    L3221.CCap_link_regions %>%
      add_title("Linked regions to carbon constraints", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_CCap_Constraint",
                     "policy/A_CCap_Region") ->
      L3221.CCap_link_regions

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

    L3221.CCap_GHG_Link %>%
      add_title("GHG Link for carbon/GHG caps", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_CCap_Constraint",
                     "policy/A_CTax_Link") ->
    L3221.CCap_GHG_Link

    return_data(L3221.CCap_constraint, L3221.CCap_link_regions,
                L3221.CCap_tech, L3221.CCap_tranTech, L3221.CCap_resource,
                L3221.CCap_GHG_Link)
  } else {
    stop("Unknown command")
  }
}

