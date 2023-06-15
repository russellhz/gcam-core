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
             FILE = "policy/A_Policy_RES_PMultiplier",
             "L226.StubTechCoef_elecownuse",
             "L226.StubTechCoef_electd",
             "L2233.GlobalTechEff_elec_cool",
             "L222.GlobalTechCoef_en",
             "L201.BaseGDP_Scen",
             paste0("L201.LaborProductivity_gSSP", seq(1, 5)),
             paste0("L201.Pop_gSSP", seq(1, 5)),
             FILE = "policy/A_OutputsByTech"
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L301.policy_port_stnd",
             "L301.policy_RES_coefs",
             "L301.RES_secout",
             "L301.pmultiplier",
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
    A_Policy_RES_PMultiplier <- get_data(all_data, "policy/A_Policy_RES_PMultiplier")
    L226.StubTechCoef_elecownuse <- get_data(all_data, "L226.StubTechCoef_elecownuse")
    L226.StubTechCoef_electd <- get_data(all_data, "L226.StubTechCoef_electd")
    L222.GlobalTechCoef_en <- get_data(all_data, "L222.GlobalTechCoef_en")
    L2233.GlobalTechEff_elec_cool <- get_data(all_data, "L2233.GlobalTechEff_elec_cool")

    L201.BaseGDP_Scen <- get_data(all_data, "L201.BaseGDP_Scen")
    L201.LaborProductivity <-  get_data(all_data, paste0("L201.LaborProductivity_g", socioeconomics.BASE_GDP_SCENARIO))
    L201.Pop <-  get_data(all_data, paste0("L201.Pop_g", socioeconomics.BASE_GDP_SCENARIO))
    A_OutputsByTech <- get_data(all_data, "policy/A_OutputsByTech") %>%
      gather_years()

    # 1a. Perform any gdp intensity calculations, if needed
    if (any(!is.na(A_Policy_Constraints$GDPIntensity_BaseYear))){
      GDP_Intensity_targets <- A_Policy_Constraints %>%
        filter(!is.na(GDPIntensity_BaseYear))

      # First calculate GDP series - we have baseGDP, growth rate in perCapitaGDP and population
      L301.GDP <- L201.BaseGDP_Scen %>%
        mutate(year = min(MODEL_BASE_YEARS)) %>%
        bind_rows(L201.LaborProductivity) %>%
        arrange(region, year) %>%
        left_join_error_no_match(L201.Pop, by = c("region", "year")) %>%
        mutate(baseGDPperCapita = baseGDP / totalPop) %>%
        group_by(region) %>%
        mutate(year_diff = year - lag(year),
               multiplier = if_else(is.na(laborproductivity), 1,(laborproductivity + 1) ^ year_diff),
               multiplier = cumprod(multiplier),
               baseGDPperCapita = baseGDPperCapita[year == min(MODEL_BASE_YEARS)]) %>%
        ungroup %>%
        mutate(GDP = totalPop * baseGDPperCapita * multiplier) %>%
        select(region, year, GDP)

      # Next get the energy technologies that we want to constraint
      L301.energy_consumption <- A_Policy_Constraints_Techs %>%
        tidyr::pivot_longer(cols = c(input.tax, input.subsidy),
                            names_to = "policy", values_to = "policy.portfolio.standard") %>%
        semi_join(GDP_Intensity_targets, by = c("region", "policy.portfolio.standard")) %>%
        left_join_error_no_match(select(GDP_Intensity_targets, region, policy.portfolio.standard, GDPIntensity_BaseYear),
                                 by = c("region", "policy.portfolio.standard"))

      # Calculate the energy output of the techs in the base year
      L301.baseEnergy <- L301.energy_consumption %>%
        # Filter out techs we know don't exist in base year
        # Want this to be hard-coded so that it throws error for other cases
        filter(!(region == "Australia_NZ" &
                   GDPIntensity_BaseYear == 2015 &
                   grepl("elec_Gen", supplysector))) %>%
        left_join_error_no_match(A_OutputsByTech, by = c("region", "supplysector" = "sector", "subsector",
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
        select(xml, region, market, policy.portfolio.standard, policyType, year, constraint)

    }

    # 1b. Extend targets to all desired years
    # Filter out NA years and interpolate between non-NA years
    L301.ceilings_floors_NA <- bind_rows(A_Policy_Constraints, A_Policy_RES_Coefs) %>%
      gather_years(value_col = "constraint") %>%
      filter(!is.na(constraint),
             is.na(GDPIntensity_BaseYear)) %>%
      select(-GDPIntensity_BaseYear)

    if (exists("L301.futureEnergy")){
      L301.ceilings_floors_NA <- bind_rows(L301.ceilings_floors_NA, L301.futureEnergy)
    }

    L301.ceilings_floors_NA <- L301.ceilings_floors_NA %>%
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
                       res.secondary.output, output.ratio, calculate_elec_losses, calc_fossil_conversion),
               year = seq(min(year), max(year), 5)) %>%
      ungroup %>%
      arrange(year)

    # Calculate electricity losses if specified
    secout_elec_losses <- L301.RES_secout %>%
      filter(calculate_elec_losses == 1) %>%
      select(-calc_fossil_conversion)

    L301.RES_secout <- L301.RES_secout %>%
      filter(is.na(calculate_elec_losses)) %>%
      select(-calculate_elec_losses)

    # Calculate fossil equivalent losses if specified
    secout_fossil_conv <- L301.RES_secout %>%
      filter(calc_fossil_conversion == 1)

    L301.RES_secout <- L301.RES_secout %>%
      filter(is.na(calc_fossil_conversion)) %>%
      select(-calc_fossil_conversion)


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

    if (nrow(secout_fossil_conv) > 0){
      # First get coal efficiency, which will be used for all elec techs
      coal_eff <- L2233.GlobalTechEff_elec_cool %>%
        filter(technology == "coal (conv pul) (recirculating)") %>%
        select(year, efficiency)

      secout_fossil_conv_ELEC <-  secout_fossil_conv %>%
        filter(grepl("elec", supplysector)) %>%
        left_join_error_no_match(coal_eff, by = "year") %>%
        mutate(output.ratio =  1/ efficiency )%>%
        select(region, supplysector, subsector, stub.technology, res.secondary.output, output.ratio, year)

      # All remaining NAs are assumed to be non-energy use of fossil fuels (feedstocks)
      # Assign the same output as constraint, but with
      secout_fossil_conv_NONENERGY <-  secout_fossil_conv %>%
        filter(!grepl("elec", supplysector)) %>%
        # Need to add some columns to join in policy coefficients and fossil losses
        mutate(
          coeff_sector = case_when(
            subsector == "gas" ~ "regional natural gas",
            subsector == "refined liquids" ~ "regional oil",
            subsector == "refined liquids bunkers" ~ "regional oil",
            subsector == "coal" ~ "regional coal"
        ),
          tran_subsector = case_when(
            subsector == "gas" ~ "natural gas",
            subsector == "refined liquids" ~ "oil refining",
            subsector == "refined liquids bunkers" ~ "oil refining",
            subsector == "coal" ~ "" # no coal losses
        )
        ) %>%
        # Join in policy coefficients
        left_join_keep_first_only(L301.ceilings_floors,
                  by = c("region", "year", "res.secondary.output" = "policy.portfolio.standard",
                         "coeff_sector" = "supplysector")) %>%
        # Join in fossil losses
        left_join(L222.GlobalTechCoef_en,
                  by = c("year", "tran_subsector" = "subsector.name", "coeff_sector" = "minicam.energy.input")) %>%
        mutate(output.ratio = if_else(is.na(coefficient), constraint, coefficient * constraint)) %>%
        select(region, supplysector, subsector = subsector.x, stub.technology = stub.technology.x, res.secondary.output, output.ratio, year)

      L301.RES_secout <- L301.RES_secout %>%
        bind_rows(secout_fossil_conv_ELEC,
                  secout_fossil_conv_NONENERGY)

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

    # 7. Add price multipliers
    L301.pmultiplier <- A_Policy_RES_PMultiplier %>%
      gather_years(value_col = "pMultiplier") %>%
      filter(!is.na(pMultiplier)) %>%
      # Leaving grouped on purpose here
      group_by(region, market, policy.portfolio.standard, policyType,
               supplysector, subsector, stub.technology) %>%
      # Interpolates between min and max years for each region/policy combo
      complete(nesting(xml, region, market, policy.portfolio.standard, policyType,
                       supplysector, subsector, stub.technology),
               year = seq(min(year), max(year), 5)) %>%
      mutate(pMultiplier = approx_fun(year, pMultiplier)) %>%
      ungroup %>%
      select(region, supplysector, subsector, stub.technology, res.secondary.output = policy.portfolio.standard, pMultiplier, year)

    # 8. Make mapping of policies to xml file names
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
      add_precursors("policy/A_Policy_RES_SecOut",
                     "L222.GlobalTechCoef_en",
                     "L2233.GlobalTechEff_elec_cool") ->
      L301.RES_secout

    L301.pmultiplier  %>%
      add_title("Price multipliers for RES markets", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_Policy_RES_PMultiplier") ->
      L301.pmultiplier

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
                L301.pmultiplier,
                L301.input_tax,
                L301.input_subsidy,
                L301.XML_policy_map)
  } else {
    stop("Unknown command")
  }
}

