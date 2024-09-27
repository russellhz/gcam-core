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
    return(c(FILE = "policy/A_energy_constraints",
             FILE = "policy/mappings/policy_tech_mappings",
             FILE = "policy/mappings/market_region_mappings",
             "L226.StubTechCoef_elecownuse",
             "L226.StubTechCoef_electd",
             "L2233.GlobalTechEff_elec_cool",
             "L222.GlobalTechCoef_en",
             "L201.GDP_Scen",
             "L201.GDP_GCAM_IC",
             "L221.StubTech_en",
             "L222.StubTech_en",
             "L224.StubTech_heat",
             "L223.StubTech_elec",
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
             "L225.StubTech_h2",
             "L239.PrimaryConsKeyword_en",
             "L2392.PrimaryConsKeyword_en_NG",
             "L221.StubTechCalInput_bioOil") )
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L301.policy_port_stnd",
             "L301.XML_policy_map",
             "L301.input_tax",
             "L301.input_subsidy",
             "L301.input_tax_NG",
             "L301.input_subsidy_NG"
             ))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs -------------------------
    market_region_mapping <- get_data(all_data, "policy/mappings/market_region_mappings")

    A_energy_constraints <- get_data(all_data, "policy/A_energy_constraints") %>%
      left_join(market_region_mapping, by = "market") %>%
      mutate(region = if_else(is.na(region), market, region))
    policy_tech_mappings <- get_data(all_data, "policy/mappings/policy_tech_mappings")
    L226.StubTechCoef_elecownuse <- get_data(all_data, "L226.StubTechCoef_elecownuse")
    L226.StubTechCoef_electd <- get_data(all_data, "L226.StubTechCoef_electd")
    L222.GlobalTechCoef_en <- get_data(all_data, "L222.GlobalTechCoef_en")
    L2233.GlobalTechEff_elec_cool <- get_data(all_data, "L2233.GlobalTechEff_elec_cool")

    L201.GDP_GCAM_IC <- get_data(all_data, "L201.GDP_GCAM_IC")

    L2392.PrimaryConsKeyword_en_NG <- get_data(all_data, "L2392.PrimaryConsKeyword_en_NG")
    L301.StubTech_All <- bind_rows(distinct(bind_rows(get_data(all_data, "L221.StubTech_en"),
                                   # Not all biomassOil techs are in the stubtech
                                   get_data(all_data, "L221.StubTechCalInput_bioOil")), region, supplysector, subsector, stub.technology),
                                    get_data(all_data, "L222.StubTech_en"),
                                   get_data(all_data, "L223.StubTech_elec"),
                                    get_data(all_data, "L2233.StubTech_elec_cool"),
                                    get_data(all_data, "L224.StubTech_heat"),
                                    get_data(all_data, "L225.StubTech_h2"),
                                    get_data(all_data, "L226.StubTech_en"),
                                    get_data(all_data, "L232.StubTech_ind"),
                                    get_data(all_data, "L2321.StubTech_cement"),
                                    get_data(all_data, "L2322.StubTech_Fert"),
                                    get_data(all_data, "L2323.StubTech_iron_steel"),
                                    get_data(all_data, "L2324.StubTech_Off_road"),
                                    get_data(all_data, "L2325.StubTech_chemical"),
                                    get_data(all_data, "L2326.StubTech_aluminum"),
                                    get_data(all_data, "L244.StubTech_bld"),
                                    get_data(all_data, "L239.PrimaryConsKeyword_en") %>%
                                     select(-primary.consumption, -year) %>%
                                     distinct() %>%
                                     rename(stub.technology = technology)
                                   )

    # 1a. Perform any gdp intensity calculations, if needed -------------------
    if (any(!is.na(A_energy_constraints$GDPIntensity_BaseYear))){
      GDP_Intensity_targets <- A_energy_constraints %>%
        filter(!is.na(GDPIntensity_BaseYear))

      # First calculate GDP series - we have baseGDP, growth rate in perCapitaGDP and population
      L301.GDP <- L201.GDP_GCAM_IC

      # Next get the energy technologies that we want to constraint
      L301.energy_consumption <- select(GDP_Intensity_targets, region, policy.portfolio.standard, GDPIntensity_BaseYear, tech_mapping) %>%
        left_join(policy_tech_mappings, by = c("tech_mapping")) %>%
        select(-tech_mapping)

      # Calculate the energy output of the techs in the base year
      L301.baseEnergy <- L301.energy_consumption %>%
        # Filter out techs that don't exist in base year
        inner_join(A_OutputsByTech, by = c("region", "supplysector" = "sector", "subsector",
                                           "stub.technology" = "technology", "GDPIntensity_BaseYear" = "year"))

      # Add in GDP and calculate base GDP Intensity
      L301.baseIntensity <- L301.baseEnergy %>%
        group_by(region, policy.portfolio.standard, GDPIntensity_BaseYear) %>%
        summarise(value = sum(value)) %>%
        ungroup %>%
        left_join_error_no_match(L301.GDP, by = c("region", "GDPIntensity_BaseYear" = "year")) %>%
        mutate(GDP_intensity = value / GDP) %>%
        select(region, policy.portfolio.standard, GDPIntensity_BaseYear, GDP_intensity)

      # Future GDP intensities
      L301.futureIntensity <- GDP_Intensity_targets %>%
        gather_years() %>%
        na.omit() %>%
        left_join_error_no_match(L301.baseIntensity,
                                 by = c("region", "policy.portfolio.standard", "GDPIntensity_BaseYear")) %>%
        mutate(GDP_intensity = GDP_intensity * (100 + value) / 100)

      # Calculate future energy constraints based on GDP intensities
      L301.futureEnergy <- L301.futureIntensity %>%
        left_join_error_no_match(L301.GDP, by = c("region", "year")) %>%
        mutate(constraint = GDP_intensity * GDP) %>%
        select(xml, region, market, policy.portfolio.standard, policyType,tech_mapping, year, constraint)

    }

    # 1b. Extend targets to all desired years --------------------
    # Filter out NA years and interpolate between non-NA years
    L301.ceilings_floors_NA <- A_energy_constraints %>%
      gather_years(value_col = "constraint") %>%
      filter(!is.na(constraint),
             is.na(GDPIntensity_BaseYear)) %>%
      select(-GDPIntensity_BaseYear)

    if (exists("L301.futureEnergy")){
      L301.ceilings_floors_NA <- bind_rows(L301.ceilings_floors_NA, L301.futureEnergy)
    }

    L301.ceilings_floors_NA <- L301.ceilings_floors_NA %>%
      # Leaving grouped on purpose here
      group_by(region, market, policy.portfolio.standard, policyType, tech_mapping ) %>%
      # Interpolates between min and max years for each region/policy combo
      complete(nesting(xml, region, market, policy.portfolio.standard, policyType, tech_mapping ),
               year = seq(min(year), max(year), 5))


    # Separate out groups with only 1 value since they get turned into NAs with approx_fun
    L301.ceilings_floors_n1 <- L301.ceilings_floors_NA %>%
      filter(dplyr::n() == 1) %>%
      ungroup

    L301.ceilings_floors <- L301.ceilings_floors_NA %>%
      filter(dplyr::n() > 1) %>%
      mutate(constraint = approx_fun(year, constraint)) %>%
      ungroup %>%
      bind_rows(L301.ceilings_floors_n1) %>%
      arrange(region, policyType, year) %>%
      left_join(policy_tech_mappings, by = "tech_mapping") %>%
      select(region, market, policy.portfolio.standard, policyType, supplysector, subsector, stub.technology, year, constraint)

    # Need to drop techs that don't exist, write them out here
    tech_remove <- L301.ceilings_floors %>%
      anti_join(L301.StubTech_All, by = c("region", "supplysector", "subsector", "stub.technology")) %>%
      distinct(region, supplysector, subsector, stub.technology)

    if (nrow(tech_remove) > 0){
      print("Constraint removed for the following sectors:")
      for(i in 1:nrow(tech_remove)){
        print(paste(tech_remove[i,], collapse = "---"))
      }
      L301.ceilings_floors <- L301.ceilings_floors %>%
        anti_join(tech_remove, by = c("region", "supplysector", "subsector", "stub.technology"))
    }

    # 2. Create policy portfolio standard tables --------------------
    L301.policy_port_stnd <- L301.ceilings_floors %>%
      mutate(constraint = if_else(policyType == "RES",
                                  1,
                                  constraint)) %>%
      select(LEVEL2_DATA_NAMES[["PortfolioStdConstraint"]]) %>%
      distinct()

    # 3. Create input tax tables - apply to all model years because of vintages --------------------
    L301.input_tax <- L301.ceilings_floors %>%
      semi_join(A_energy_constraints %>% filter(policyType == "tax"),
                by = c("region", "market", "policy.portfolio.standard", "policyType")) %>%
      distinct(region, input.tax = policy.portfolio.standard, supplysector, subsector, stub.technology) %>%
      repeat_add_columns(tibble(year = c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS)))

    # 4. Create input subsidy tables - apply to all model years because of vintages --------------------
    L301.input_subsidy <- L301.ceilings_floors %>%
      semi_join(A_energy_constraints %>% filter(policyType == "subsidy"),
                by = c("region", "market", "policy.portfolio.standard", "policyType")) %>%
      distinct(region, input.subsidy = policy.portfolio.standard, supplysector, subsector, stub.technology) %>%
      repeat_add_columns(tibble(year = c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS)))

    # 5. Make mapping of policies to xml file names --------------------
    L301.XML_policy_map <- distinct(A_energy_constraints, xml, policy.portfolio.standard, market) %>%
      mutate(xml = if_else(grepl(".xml", xml), xml, paste0(xml, ".xml")))

    # Produce NG outputs ------------

    L301.PrimaryConsKeyword_en_NG <- L2392.PrimaryConsKeyword_en_NG %>%
      select(-primary.consumption, -year) %>% distinct()

    L301.input_tax %>%
      semi_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      left_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      rename(subsector0 = subsector, subsector = subsector.y) %>%
      select(-stub.technology) %>%
      add_title("Technologies to apply constraint to", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_renewable_energy_standards") ->
      L301.input_tax_NG

    L301.input_subsidy %>%
      semi_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      left_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      rename(subsector0 = subsector, subsector = subsector.y) %>%
      select(-stub.technology) %>%
      add_title("Technologies to apply constraint to", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_renewable_energy_standards") ->
      L301.input_subsidy_NG

    # Produce other outputs ------------

    L301.policy_port_stnd %>%
      add_title("Policy names and constraints", overwrite = T) %>%
      add_units("EJ (for taxes) or NA (for RES)") %>%
      add_precursors("policy/A_energy_constraints",
                     "policy/A_renewable_energy_standards") ->
      L301.policy_port_stnd

    L301.input_tax %>%
      anti_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      add_title("Technologies to apply constraint to", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_renewable_energy_standards") ->
      L301.input_tax

    L301.input_subsidy %>%
      anti_join(L301.PrimaryConsKeyword_en_NG,
                by = c("region", "supplysector", "subsector" = "subsector0")) %>%
      add_title("Technologies to apply constraint to", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_renewable_energy_standards") ->
      L301.input_subsidy

    L301.XML_policy_map %>%
      add_title("Mapping of policy names to xml", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_energy_constraints",
                     "policy/A_renewable_energy_standards") ->
      L301.XML_policy_map

    return_data(L301.policy_port_stnd,
                L301.input_tax,
                L301.input_subsidy,
                L301.XML_policy_map,
                L301.input_tax_NG,
                L301.input_subsidy_NG)
  } else {
    stop("Unknown command")
  }
}

