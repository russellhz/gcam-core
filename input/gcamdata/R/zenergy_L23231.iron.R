# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L23231.iron
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for new iron sector
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for iron sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr gather spread
#' @author Russell Horowitz Feb 26 2025
module_energy_L23231.iron <- function(command, ...) {

  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     FILE = "energy/calibrated_techs",
                     FILE = "energy/A323.sector_mapping",
                     FILE = "energy/A3231.sector",
                     FILE = "energy/A3231.subsector_logit",
                     FILE = "energy/A3231.subsector_shrwt",
                     FILE = "energy/A3231.globaltech_coef",
                     FILE = "energy/A3231.globaltech_cost",
                     FILE = "energy/A3231.globaltech_shrwt",
                     FILE = "energy/A3231.globaltech_retirement",
                     FILE = "energy/A3232.globaltech_coef",
                     FILE = "energy/A3232.globaltech_cost",
                     "L2323.StubTechProd_iron_steel",
                     "L2323.StubTechCoef_iron_steel",
                     "L2323.StubTechCost_iron_steel")

  MODULE_OUTPUTS <- c("L23231.Supplysector_iron",
                      "L23231.FinalEnergyKeyword_iron",
                      "L23231.SubsectorLogit_iron",
                      "L23231.SubsectorShrwtFllt_iron",
                      "L23231.SubsectorInterp_iron",
                      "L23231.StubTech_iron",
                      "L23231.GlobalTechShrwt_iron",
                      "L23231.GlobalTechCoef_iron",
                      "L23231.GlobalTechCost_iron",
                      "L23231.GlobalTechTrackCapital_iron",
                      "L23231.GlobalTechShutdown_en",
                      "L23231.GlobalTechSCurve_en",
                      "L23231.GlobalTechLifetime_en",
                      "L23231.GlobalTechProfitShutdown_en",
                      "L23231.StubTechProd_iron",
                      "L23231.StubTechCoef_iron",
                      "L23231.StubTechCost_iron",
                      "L23231.StubTechShrwt_iron")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ---------------------
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = T)

    # 1a. Supplysector information --------------------
    # L23231.Supplysector_iron: Supply sector information for iron sector
    A3231.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L23231.Supplysector_iron

    # L23231.FinalEnergyKeyword_iron: Supply sector keywords for iron sector
    A3231.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]], GCAM_region_names) %>%
      na.omit ->
      L23231.FinalEnergyKeyword_iron

    # 1b. Subsector information --------------------
    # L23231.SubsectorLogit_iron: Subsector logit exponents of iron sector
    A3231.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L23231.SubsectorLogit_iron

    # and L23231.SubsectorShrwtFllt_iron: Subsector shareweights of iron sector
    A3231.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) ->
      L23231.SubsectorShrwtFllt_iron

    # L23231.SubsectorInterp_iron: Subsector shareweight interpolation of iron sector
    L23231.SubsectorInterp_iron <- L23231.SubsectorShrwtFllt_iron %>%
      distinct(region, supplysector, subsector) %>%
      mutate(apply.to = "share-weight",
             from.year = MODEL_FINAL_BASE_YEAR,
             to.year = max(MODEL_FUTURE_YEARS),
             interpolation.function = "linear")

    # 1c. Stub Technology information --------------------
    # L23231.StubTech_iron: Identification of stub technologies of iron
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A3231.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L23231.StubTech_iron

    # 2. Global Technology information --------------------
    # L23231.GlobalTechShrwt_iron: Shareweights of global iron technologies
    A3231.globaltech_shrwt %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight") ->
      L23231.GlobalTechShrwt_iron

    # L23231.GlobalTechCoef_iron: Energy inputs and coefficients of iron technologies
    A3231.globaltech_coef %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, value, rule = 1),
             coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L23231.GlobalTechCoef_iron

    # Retirement information
    A3231.globaltech_retirement %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      # set years based on A3231.globaltech_retirement
      set_years() %>%
      mutate(year = as.integer(year)) ->
      L23231.globaltech_retirement_base

    # Copies first future year retirment information into all future years and appends back onto base year
    L23231.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) ->
      L23231.globaltech_retirement_future

    # filters base years from original and then appends future years
    L23231.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      bind_rows(L23231.globaltech_retirement_future) ->
      L23231.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # This section checks L23231.globaltech_retirement for each of these functions and creates a separate level 2 file for each
    # All of these options have different headers, and all are allowed
    if(any(!is.na(L23231.globaltech_retirement$shutdown.rate))) {
      L23231.globaltech_retirement %>%
        filter(!is.na(L23231.globaltech_retirement$shutdown.rate)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "shutdown.rate") ->
        L23231.GlobalTechShutdown_en
    }

    if(any(!is.na(L23231.globaltech_retirement$half.life))) {
      L23231.globaltech_retirement %>%
        filter(!is.na(L23231.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
        L23231.GlobalTechSCurve_en
    }

    # L23231.GlobalTechLifetime_en: Global tech lifetime
    if(any(is.na(L23231.globaltech_retirement$shutdown.rate) & is.na(L23231.globaltech_retirement$half.life))) {
      L23231.globaltech_retirement %>%
        filter(is.na(L23231.globaltech_retirement$shutdown.rate) & is.na(L23231.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime") ->
        L23231.GlobalTechLifetime_en
    }

    # L23231.GlobalTechProfitShutdown_en: Global tech profit shutdown decider and parameters
    if(any(!is.na(L23231.globaltech_retirement$median.shutdown.point))) {
      L23231.globaltech_retirement %>%
        filter(!is.na(L23231.globaltech_retirement$median.shutdown.point)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
        L23231.GlobalTechProfitShutdown_en
    }

    # L23231.GlobalTechCost_iron: Non-energy costs of global iron manufacturing technologies
    A3231.globaltech_cost %>%
      filter(minicam.non.energy.input == "non-energy") %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.non.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, value, rule = 1),
             input.cost = round(input.cost, energy.DIGITS_COST)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]]) ->
      L23231.GlobalTechCost_iron

    FCR <- (socioeconomics.DEFAULT_INTEREST_RATE * (1+socioeconomics.DEFAULT_INTEREST_RATE)^socioeconomics.INDUSTRY_CAP_PAYMENTS) /
      ((1+socioeconomics.DEFAULT_INTEREST_RATE)^socioeconomics.INDUSTRY_CAP_PAYMENTS -1)
    L23231.GlobalTechCost_iron %>%
      # note: the units for iron output / costs will yield a dollar
      # amount of million 1975$, the rest of the capital market will be in billion 1975$
      # so we need to include the unit conversion here to make it consistent
      mutate(capital.coef = socioeconomics.INDUSTRY_CAPITAL_RATIO / FCR / 1000,
             tracking.market = socioeconomics.EN_CAPITAL_MARKET_NAME,
             # vintaging is active so no need for depreciation
             depreciation.rate = 0) %>%
      select(LEVEL2_DATA_NAMES[['GlobalTechTrackCapital']]) ->
      L23231.GlobalTechTrackCapital_iron

    # 3a. Calibrated Production --------------------
    # derive iron production from iron and steel production
    L23231.StubTechProd_iron <- L2323.StubTechProd_iron_steel %>%
      left_join_error_no_match(A323.sector_mapping, by = c("supplysector" = "iron_and_steel")) %>%
      select(-supplysector, -steel, -subsector, -stub.technology) %>%
      rename(supplysector = iron) %>%
      mutate(subsector = supplysector, stub.technology = supplysector)

    L23231.StubTechShrwt_iron <- L23231.StubTechProd_iron %>%
      filter(supplysector == "DRI_coal", year == MODEL_FINAL_BASE_YEAR, calOutputValue > 0) %>%
      distinct(region, supplysector, subsector, stub.technology) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS, share.weight = 1))

    # 3b. Calibrated coefficients --------------------
    # Take iron_and_steel coefficients, rewrite to iron, subtract steel coefficients
    L23231.StubTechCoef_iron <-  L2323.StubTechCoef_iron_steel %>%
      # first rewrite to iron
      left_join_error_no_match(A323.sector_mapping, by = c("supplysector" = "iron_and_steel")) %>%
      select(-supplysector, -subsector, -stub.technology) %>%
      rename(supplysector = iron) %>%
      mutate(subsector = supplysector, stub.technology = supplysector) %>%
      # now subtract out global tech coefs for steel
      left_join(select(A3232.globaltech_coef, steel = supplysector, minicam.energy.input, steel_coef = `2010`),
                by = c("steel", "minicam.energy.input")) %>%
      tidyr::replace_na(list(steel_coef = 0)) %>%
      mutate(coefficient = round(coefficient - steel_coef, energy.DIGITS_COEFFICIENT)) %>%
      select(-steel, -steel_coef)

    stopifnot(all(L23231.StubTechCoef_iron$coefficient >= 0))

    # 3c. Calibrated costs --------------------
    # Take calibrated iron_and_steel costs, rewrite to iron, subtract steel coefficients
    L23231.StubTechCost_iron <- L2323.StubTechCost_iron_steel %>%
      # first rewrite to iron
      left_join_error_no_match(A323.sector_mapping, by = c("supplysector" = "iron_and_steel")) %>%
      select(-supplysector, -subsector, -stub.technology) %>%
      rename(supplysector = iron) %>%
      mutate(subsector = supplysector, stub.technology = supplysector) %>%
      # now subtract out global tech costs for steel
      left_join(select(A3232.globaltech_cost, steel = supplysector, minicam.non.energy.input, steel_cost = `2020`),
                by = c("steel", "minicam.non.energy.input")) %>%
      mutate(input.cost = if_else(is.na(steel_cost), input.cost, round(input.cost - steel_cost, energy.DIGITS_COST))) %>%
      select(-steel, -steel_cost)

    stopifnot(all(L23231.StubTechCost_iron$input.cost >= 0))

    # Produce outputs ===================================================
    L23231.Supplysector_iron %>%
      add_title("Supply sector information for iron sector", overwrite = T) %>%
      add_units("NA")  ->
      L23231.Supplysector_iron

    L23231.FinalEnergyKeyword_iron %>%
      add_title("Supply sector keywords for iron sector") %>%
      add_units("NA") %>%
      add_comments("For iron sector, the supply sector final energy keywords from A323.sector are expended into all GCAM regions") %>%
      add_precursors("energy/A323.sector", "common/GCAM_region_names") ->
      L23231.FinalEnergyKeyword_iron

    L23231.SubsectorLogit_iron %>%
      add_title("Subsector logit exponents of iron sector") %>%
      add_units("Unitless") %>%
      add_comments("For iron sector, the subsector logit exponents from A323.subsector_logit are expanded into all GCAM regions") %>%
      add_precursors("energy/A323.subsector_logit", "common/GCAM_region_names") ->
      L23231.SubsectorLogit_iron

    L23231.SubsectorShrwtFllt_iron %>%
      add_title("Subsector shareweights of iron sector") %>%
      add_units("unitless") %>%
      add_comments("For iron sector, the subsector shareweights from A323.subsector_shrwt are expanded into all GCAM regions") %>%
      add_precursors("energy/A323.subsector_shrwt", "common/GCAM_region_names") ->
      L23231.SubsectorShrwtFllt_iron

    L23231.SubsectorInterp_iron %>%
      add_title("Subsector shareweight interpolation of iron sector") %>%
      add_units("NA") %>%
      add_comments("For iron sector, the subsector shareweight interpolation function infromation from A323.subsector_interp is expanded into all GCAM regions") %>%
      add_precursors("L1323.SubsectorInterp_iron") ->
      L23231.SubsectorInterp_iron

    L23231.StubTech_iron %>%
      add_title("Identification of stub technologies of iron and steel") %>%
      add_units("NA") %>%
      add_comments("For iron sector, the stub technologies from A323.globaltech_shrwt are expanded into all GCAM regions") %>%
      add_precursors("energy/A323.globaltech_shrwt", "common/GCAM_region_names") ->
      L23231.StubTech_iron

    L23231.GlobalTechShrwt_iron %>%
      add_title("Shareweights of global iron technologies") %>%
      add_units("Unitless") %>%
      add_comments("For iron sector, the share weights from A323.globaltech_shrwt are interpolated into all base years and future years") %>%
      add_precursors("energy/A323.globaltech_shrwt") ->
      L23231.GlobalTechShrwt_iron

    L23231.GlobalTechCoef_iron %>%
      add_title("Energy inputs and coefficients of iron technologies") %>%
      add_units("scrap input is unitless (Mt scrap per Mt steel); all others are GJ per kg (EJ of energy per Mt of steel)") %>%
      add_comments("For iron sector, the energy use coefficients from A323.globaltech_coef are interpolated into all model years") %>%
      add_precursors("energy/A323.globaltech_coef") ->
      L23231.GlobalTechCoef_iron

    L23231.GlobalTechCost_iron %>%
      add_title("Non-energy costs of global iron manufacturing technologies") %>%
      add_units("1975$/kg for supplysector iron; 1975$/GJ for supplysector process heat iron") %>%
      add_comments("For iron sector, the Non-energy costs of global iron manufacturing technologies are calculated then adjusted with CCS to include CO2 capture costs") %>%
      add_precursors("energy/A323.globaltech_cost", "energy/A323.globaltech_co2capture", "energy/A323.globaltech_coef") ->
      L23231.GlobalTechCost_iron

    L23231.GlobalTechTrackCapital_iron %>%
      add_title("Convert non-energy inputs to track the annual capital investments.") %>%
      add_units(("Coefficients")) %>%
      add_comments("Track capital investments for purposes of macro economic calculations") %>%
      same_precursors_as(L23231.GlobalTechCost_iron) ->
      L23231.GlobalTechTrackCapital_iron

    L23231.StubTechCost_iron %>%
      add_title("Regionally varying non-energy costs of iron manufacturing technologies") %>%
      add_units("1975$/kg for supplysector iron") %>%
      add_comments("For iron sector, the non-energy costs of regional iron manufacturing technologies are obtained from transition zero global cost tracker database and then adjusted with capital costs and CCS to include CO2 capture costs") %>%
      add_precursors("energy/A323.capital_cost_adders", "energy/A323.ccs_adders", "energy/TZ_production_costs","energy/mappings/TZ_cost_gcam_mapping",
                     "energy/mappings/TZ_cost_oecd_mapping") ->
      L23231.StubTechCost_iron

    if(exists("L23231.GlobalTechShutdown_en")) {
      L23231.GlobalTechShutdown_en %>%
        add_title("Global tech lifetime for techs with shutdown rate") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that uses a phased retirement function") %>%
        add_precursors("energy/A323.globaltech_retirement") ->
        L23231.GlobalTechShutdown_en
    } else {
      missing_data() ->
        L23231.GlobalTechShutdown_en
    }

    if(exists("L23231.GlobalTechSCurve_en")) {
      L23231.GlobalTechSCurve_en %>%
        add_title("Global tech lifetime for techs with s-curve retirement function") %>%
        add_units("Lifetime in years, half-life in years") %>%
        add_comments("Filters for any technology that uses an S-curve retirement function") %>%
        add_precursors("energy/A323.globaltech_retirement") ->
        L23231.GlobalTechSCurve_en
    } else {
      missing_data()  ->
        L23231.GlobalTechSCurve_en
    }

    if(exists("L23231.GlobalTechLifetime_en")) {
      L23231.GlobalTechLifetime_en %>%
        add_title("Global tech lifetime for any technology with no retirement function") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that has no phased or S-curve retirement function, empty by default.") %>%
        add_precursors("energy/A323.globaltech_retirement") ->
        L23231.GlobalTechLifetime_en
    } else {
      missing_data() ->
        L23231.GlobalTechLifetime_en
    }

    if(exists("L23231.GlobalTechProfitShutdown_en")) {
      L23231.GlobalTechProfitShutdown_en %>%
        add_title("Global tech profit shutdown decider and parameters") %>%
        add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
        add_comments("Filters for any technologies that use a profit-based shutdown parameter") %>%
        add_precursors("energy/A323.globaltech_retirement") ->
        L23231.GlobalTechProfitShutdown_en
    } else {
      missing_data() ->
        L23231.GlobalTechProfitShutdown_en
    }

    L23231.StubTechProd_iron %>%
      add_title("calibrated iron production") %>%
      add_units("Mt") %>%
      add_comments("Values are calculated using L1323.out_Mt_R_iron_Yh then added GCAM region information and supplysector, subsector, and technology information") %>%
      add_precursors("energy/calibrated_techs", "L1323.out_Mt_R_iron_Yh", "common/GCAM_region_names") ->
      L23231.StubTechProd_iron

    L23231.StubTechCoef_iron %>%
      add_title("region-specific coefficients of iron production technologies") %>%
      add_units("scrap input is unitless (Mt scrap per Mt steel); all others are GJ per kg (EJ of energy per Mt of steel)") %>%
      add_comments("Coefficients are calculated using L1323.IO_GJkg_R_iron_F_Yh") %>%
      add_precursors("energy/calibrated_techs", "L1323.IO_GJkg_R_iron_F_Yh", "common/GCAM_region_names") ->
      L23231.StubTechCoef_iron

    return_data(L23231.Supplysector_iron, L23231.FinalEnergyKeyword_iron, L23231.SubsectorLogit_iron,
                L23231.SubsectorShrwtFllt_iron,L23231.SubsectorInterp_iron,
                L23231.StubTech_iron, L23231.GlobalTechShrwt_iron, L23231.GlobalTechCoef_iron,
                L23231.GlobalTechTrackCapital_iron, L23231.GlobalTechCost_iron, L23231.GlobalTechShutdown_en,
                L23231.GlobalTechSCurve_en, L23231.GlobalTechLifetime_en, L23231.GlobalTechProfitShutdown_en,
                L23231.StubTechProd_iron, L23231.StubTechCoef_iron, L23231.StubTechCost_iron, L23231.StubTechShrwt_iron
                )
  } else {
    stop("Unknown command")
  }
}
