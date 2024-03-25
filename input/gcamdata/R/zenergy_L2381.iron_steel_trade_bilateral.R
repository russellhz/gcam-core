# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2381.iron_steel_trade_bilateral
#'
#' Model input for regional and (globally) traded iron and steel
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L2381.Supplysector_tra},
#'   \code{L2381.SectorUseTrialMarket_tra}, \code{L2381.SubsectorAll_tra}, \code{L2381.TechShrwt_tra},
#'   \code{L2381.TechCost_tra}, \code{L2381.TechCoef_tra}, \code{L2381.Production_tra}, \code{L2381.Supplysector_reg},
#'   \code{L2381.SubsectorAll_reg}, \code{L2381.TechShrwt_reg}, \code{L2381.TechCoef_reg}, \code{L2381.Production_reg_imp},
#'   \code{L2381.Production_reg_dom}.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else left_join mutate rename select
#' @importFrom tidyr replace_na
#' @importFrom tibble tibble
#' @author Siddarth Durga July 2022 // RLH March 2024
module_energy_L2381.iron_steel_trade_bilateral <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     FILE = "energy/A_irnstl_RegionalSector",
                     FILE = "energy/A_irnstl_RegionalSubsector",
                     FILE = "energy/A_irnstl_RegionalTechnology_bilateral",
                     FILE = "energy/A_irnstl_TradedSector_bilateral",
                     FILE = "energy/A_irnstl_TradedSubsector_bilateral",
                     FILE = "energy/A_irnstl_TradedTechnology_bilateral",
                     FILE = "energy/A_irnstl_regions",
                     "LB1092.Tradebalance_iron_steel_Mt_R_Y_OECD_bilateral",
                     "LB1092.Tradebalance_iron_steel_Mt_R_Y",
                     "L2323.StubTechProd_iron_steel",
                     FILE = "energy/A323.globaltech_secout")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2381.Supplysector_tra",
             "L2381.SectorUseTrialMarket_tra",
             "L2381.SubsectorAll_tra",
             # "L2381.SubsectorShwtClub_tra",
             "L2381.TechShrwt_tra",
             "L2381.TechCost_tra",
             "L2381.TechCoef_tra",
             "L2381.Production_tra",
             "L2381.Supplysector_reg",
             "L2381.SubsectorAll_reg",
             "L2381.TechShrwt_reg",
             "L2381.TechCoef_reg",
             "L2381.Production_reg_imp",
             "L2381.Production_reg_dom"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- region <- supplysector <- subsector <- GCAM_commodity <- GrossExp_Mt <-
      calOutputValue <- subs.share.weight <- market.name <- minicam.energy.input <-
      GrossImp_Mt <- Prod_Mt <- GCAM_region_ID <- GCAM_region <- NetExp_Mt <- Prod_bm3 <-
      NetExp_bm3 <- value <- metric <- flow <- GrossExp <- NULL # silence package check notes

    # Load required inputs  ---------------------
    get_data_list(all_data, MODULE_INPUTS)
    GCAM_region_names_OECD <- GCAM_region_names %>%
      semi_join(A_irnstl_regions %>%  filter(sector == "OECD"), by = "region")
    GCAM_region_names_nonOECD <- GCAM_region_names %>%
      anti_join(A_irnstl_regions %>%  filter(sector == "OECD"), by = "region")

    # 1. TRADED SECTOR / SUBSECTOR / TECHNOLOGY") ---------------------
    # L2381.Supplysector_tra: generic supplysector info for traded iron and steel
    # By convention, traded commodity information is contained within the USA region (could be within any)
    A_irnstl_TradedSector_bilateral$region <- gcam.USA_REGION

    # L2381.Supplysector_tra: generic supplysector info for traded iron and steel
    L2381.Supplysector_tra <- mutate(A_irnstl_TradedSector_bilateral, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"))

    # L2381.SectorUseTrialMarket_tra: Create solved markets for the traded sectors
    L2381.SectorUseTrialMarket_tra <- select(A_irnstl_TradedSector_bilateral, region, supplysector) %>%
      mutate(use.trial.market = 1)

    # L2381.SubsectorAll_tra: generic subsector info for traded iron and steel
    # Traded commodities have the region set to USA and the subsector gets the region name pre-pended
    L2381.SubsectorAll_tra_OECD <- write_to_all_regions(filter(A_irnstl_TradedSubsector_bilateral, grepl("_OECD", supplysector)),
                                                  c(LEVEL2_DATA_NAMES[["SubsectorAllTo"]], "logit.type"),
                                                  GCAM_region_names_OECD,
                                                  has_traded = TRUE)
    L2381.SubsectorAll_tra_nonOECD <- write_to_all_regions(filter(A_irnstl_TradedSubsector_bilateral, grepl("_nonOECD", supplysector)),
                                                   c(LEVEL2_DATA_NAMES[["SubsectorAllTo"]], "logit.type"),
                                                   GCAM_region_names_nonOECD,
                                                   has_traded = TRUE) %>% mutate(region = gcam.USA_REGION)
    L2381.SubsectorAll_tra <- bind_rows(L2381.SubsectorAll_tra_OECD, L2381.SubsectorAll_tra_nonOECD)


    # # L2381.SubsectorShwtClub_tra: set shareweights for subsector to 1 after 2025 for all traded steel club
    # L2381.SubsectorShwtClub_tra <- L2381.SubsectorAll_tra %>%
    #   select(region, supplysector, subsector) %>%
    #   filter(grepl("CBAM", subsector)) %>%
    #   mutate(year = 2025,
    #          share.weight = 1,
    #          from.year = 2030,
    #          apply.to = "share-weight",
    #          to.year = 2100,
    #          delete = 1,
    #          interpolation.function = "fixed")


    # Change traded iron and steel interpolation rule and to.value in countries listed in energy.IRON_STEEL.DOMESTIC_SW
    L2381.SubsectorAll_tra$interpolation.function[which(L2381.SubsectorAll_tra$subsector %in% energy.IRON_STEEL.TRADED_SW)] <- "s-curve"
    L2381.SubsectorAll_tra$to.year[which(L2381.SubsectorAll_tra$subsector %in% energy.IRON_STEEL.TRADED_SW)] <- 2300

    # Base technology-level table for several tables to be written out")
    A_irnstl_TradedTechnology_bilateral_R_Y <- repeat_add_columns(A_irnstl_TradedTechnology_bilateral,
                                                        tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names) %>%
      mutate(subsector = paste(region, subsector, sep = " "),
             technology = subsector,
             market.name = region,
             region = gcam.USA_REGION) %>%
      semi_join(L2381.SubsectorAll_tra %>%  distinct(supplysector, subsector), by = c("supplysector", "subsector"))

    # L2381.TechShrwt_tra: Share-weights of traded technologies
    L2381.TechShrwt_tra <- select(A_irnstl_TradedTechnology_bilateral_R_Y, LEVEL2_DATA_NAMES[["TechShrwt"]])

    # L2381.TechCost_tra: Costs of traded technologies
    L2381.TechCost_tra <- A_irnstl_TradedTechnology_bilateral_R_Y %>%
      mutate(minicam.non.energy.input = "trade costs") %>%
      select(LEVEL2_DATA_NAMES[["TechCost"]])

    # L2381.TechCoef_tra: Coefficient and market name of traded technologies
    L2381.TechCoef_tra <- select(A_irnstl_TradedTechnology_bilateral_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]]) %>%
      mutate(coefficient = if_else(year <= MODEL_FINAL_BASE_YEAR & minicam.energy.input != "iron and steel",
                                   0, coefficient))



    # L2381.Production_tra: Output (gross exports) of traded technologies
    L2381.GrossExports_Mt_R_Y <- left_join_error_no_match(LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
                                                           filter(metric=="exports_reval") %>%
                                                           rename(GrossExp_Mt=value,region=GCAM_region),
                                                         GCAM_region_names,
                                                         by = "region") %>%
      select(region, year, GrossExp_Mt)

    # Assume high-carbon/low-carbon export split is proportional to production
    sec.out.prodShare <- L2323.StubTechProd_iron_steel %>%
      left_join(select(A323.globaltech_secout, supplysector, subsector, stub.technology = technology, res.secondary.output),
                by = c("supplysector", "subsector", "stub.technology")) %>%
      # If no sec.out, still need to include in split, so rename to iron and steel
      tidyr::replace_na(list(res.secondary.output = "iron and steel")) %>%
      group_by(region, year, res.secondary.output) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      group_by(region, year) %>%
      mutate(prodShare = calOutputValue / sum(calOutputValue)) %>%
      ungroup %>%
      tidyr::replace_na(list(prodShare = 0)) %>%
      select(-calOutputValue)

    # Multiply sec.out share by total exports
    L2381.GrossExports_Mt_R_Y_carbonType <- L2381.GrossExports_Mt_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join(sec.out.prodShare, by = c("region", "year")) %>%
      mutate(GrossExp_Mt = GrossExp_Mt * prodShare) %>%
      rename(minicam.energy.input = res.secondary.output)

    L2381.Production_tra <- A_irnstl_TradedTechnology_bilateral_R_Y %>%
      group_by(supplysector, subsector, technology, year) %>%
      # If no sec.out, there will be only one group, otherwise it will be an input not named iron and steel
      filter(dplyr::n() == 1 | minicam.energy.input != "iron and steel") %>%
      ungroup %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L2381.GrossExports_Mt_R_Y_carbonType,
                               by = c(market.name = "region", "year", "minicam.energy.input")) %>%
      rename(calOutputValue = GrossExp_Mt) %>%
      mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
             calOutputValue = if_else(grepl("CBAM", supplysector), 0, calOutputValue),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # 2a: DOMESTIC SUPPLYSECTOR -------------------------
    # L2381.Supplysector_reg: generic supplysector info for iron and steel
    L2381.Supplysector_reg <- mutate(A_irnstl_RegionalSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"),
                           GCAM_region_names)

    # L2381.SubsectorAll_reg: generic subsector info for regional iron and steel (competing domestic prod vs intl imports)
    L2381.SubsectorAll_reg <- write_to_all_regions(A_irnstl_RegionalSubsector,
                                                  c(LEVEL2_DATA_NAMES[["SubsectorAllTo"]], "logit.type"),
                                                  GCAM_region_names)

    # Change iron and steel domestic supply interpolation rule and to.value in countries listed in energy.IRON_STEEL.DOMESTIC_SW
    L2381.SubsectorAll_reg$to.value[which(L2381.SubsectorAll_reg$region %in% energy.IRON_STEEL.DOMESTIC_SW & L2381.SubsectorAll_reg$subsector %in% c("domestic iron and steel"))] <- 1
    L2381.SubsectorAll_reg$interpolation.function[which(L2381.SubsectorAll_reg$region %in% energy.IRON_STEEL.DOMESTIC_SW & L2381.SubsectorAll_reg$subsector %in% c("domestic iron and steel"))] <- "s-curve"
    L2381.SubsectorAll_reg$to.year[which(L2381.SubsectorAll_reg$region %in% energy.IRON_STEEL.DOMESTIC_SW & L2381.SubsectorAll_reg$subsector %in% c("domestic iron and steel"))] <- 2105

    # Base technology-level table for several tables to be written out")
    A_irnstl_RegionalTechnology_bilateral_R_Y <- repeat_add_columns(A_irnstl_RegionalTechnology_bilateral,
                                                          tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names["region"]) %>%
      mutate(market.name = if_else(market.name == "regional", region, market.name))

    # L2381.TechShrwt_tra: Share-weights of traded technologies
    L2381.TechShrwt_reg <- select(A_irnstl_RegionalTechnology_bilateral_R_Y, LEVEL2_DATA_NAMES[["TechShrwt"]])

    # L2381.TechCoef_reg: Coefficient and market name of traded technologies
    L2381.TechCoef_reg <- select(A_irnstl_RegionalTechnology_bilateral_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]]) %>%
      mutate(coefficient = if_else(year <= MODEL_FINAL_BASE_YEAR & minicam.energy.input != "iron and steel" & grepl("domestic", subsector),
                                   0, coefficient))

    # 2b. L2381.Production_reg_imp: Output (flow) of gross imports ----------------------------
    # Imports are equal to the gross imports calculated in LB1092.Tradebalance_iron_steel_Mt_R_Y_OECD_bilateral
    # split between carbon levels based on global export pool
    global_exports_share <- L2381.Production_tra %>%
      mutate(Exporter_Region_Agg = stringr::str_extract(supplysector, "(?<=steel_).*")) %>%
      group_by(supplysector, Exporter_Region_Agg, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      group_by(year, Exporter_Region_Agg) %>%
      mutate(exportShare = calOutputValue / sum(calOutputValue)) %>%
      ungroup %>%
      select(-calOutputValue, -Exporter_Region_Agg)

    L2381.GrossImports_Mt_R_Y <- LB1092.Tradebalance_iron_steel_Mt_R_Y_OECD_bilateral %>%
      mutate(minicam.energy.input="iron and steel")%>%
      rename(GrossImp_Mt = value, region = GCAM_region) %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      repeat_add_columns(distinct(A_irnstl_TradedTechnology_bilateral_R_Y, supplysector)) %>%
      filter(Exporter_Region_Agg == stringr::str_extract(supplysector, "(?<=steel_).*")) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(global_exports_share, by = c("year", "supplysector")) %>%
      mutate(GrossImp_Mt = GrossImp_Mt * exportShare) %>%
      select(region, supplysector, year, GrossImp_Mt)

    L2381.Production_reg_imp <- A_irnstl_RegionalTechnology_bilateral_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "import", subsector)) %>%
      left_join_error_no_match(L2381.GrossImports_Mt_R_Y,
                               by = c("region", minicam.energy.input = "supplysector", "year")) %>%
      rename(calOutputValue = GrossImp_Mt) %>%
      mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
             calOutputValue = if_else(grepl("CBAM", technology), 0, calOutputValue),
             share.weight.year = year) %>%
      group_by(supplysector, subsector, region, year) %>%
      mutate(subs.share.weight = if_else(any(calOutputValue) > 0, 1, 0)) %>%
      ungroup %>%
      mutate(tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # 2c. L2381.Production_reg_dom: Output (flow) of domestic ---------------------
    #### DOMESTIC TECHNOLOGY OUTPUT = iron and steel PRODUCTION - GROSS EXPORTS

    A_irnstl_RegionalTechnology_bilateral_filtered <- A_irnstl_RegionalTechnology_bilateral %>%
      group_by(supplysector, subsector, technology) %>%
      # If no sec.out, there will be only one group, otherwise it will be an input not named iron and steel
      filter(dplyr::n() == 1 | minicam.energy.input != "iron and steel") %>%
      ungroup

    # Again use production share of high vs low carbon output to set domestic consumption
    L2381.DomSup_Mt_R_Y <- left_join_error_no_match(LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
                                                     filter(metric=="domestic_supply") %>%
                                                     mutate(minicam.energy.input="iron and steel")%>%
                                                     rename(DomSup_Mt=value,region=GCAM_region),
                                                   GCAM_region_names,
                                                   by = "region") %>%
      right_join(sec.out.prodShare, by = c("region", "year")) %>%
      mutate(DomSup_Mt = DomSup_Mt * prodShare) %>%
      select(region, minicam.energy.input = res.secondary.output, year, DomSup_Mt) %>%
      left_join_error_no_match(A_irnstl_RegionalTechnology_bilateral_filtered, by = "minicam.energy.input")  %>%
      select(region, supplysector, subsector, technology, year, DomSup_Mt)

    L2381.Production_reg_dom <- A_irnstl_RegionalTechnology_bilateral_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "domestic", subsector),
             minicam.energy.input == "iron and steel") %>%
      left_join_error_no_match(L2381.DomSup_Mt_R_Y,
                               by = c("region", "supplysector", "subsector", "technology", "year")) %>%
      mutate(calOutputValue = round(DomSup_Mt, energy.DIGITS_CALOUTPUT),
             share.weight.year = year) %>%
      group_by(supplysector, region, year) %>%
      mutate(subs.share.weight = if_else(any(calOutputValue) > 0, 1, 0)) %>%
      ungroup %>%
      mutate(tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # Produce outputs  ---------------------
    L2381.Supplysector_tra %>%
      add_title("Supplysector info for iron and steel", overwrite = T) %>%
      add_units("None") %>%
      add_comments("Modeled for all GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_TradedSector_bilateral") ->
      L2381.Supplysector_tra

    L2381.SectorUseTrialMarket_tra %>%
      add_title("Supplysector flag indicating to make trial markets", overwrite = T) %>%
      add_units("None") %>%
      add_comments("This helps model solution when running with iron and steel trade") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_TradedSector_bilateral") ->
      L2381.SectorUseTrialMarket_tra

    L2381.SubsectorAll_tra %>%
      add_title("Subsector info for traded iron and steel", overwrite = T) %>%
      add_units("None") %>%
      add_comments("Modeled for all GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_TradedSubsector_bilateral") ->
      L2381.SubsectorAll_tra

    L2381.TechShrwt_tra %>%
      add_title("Technology share-weights for traded iron and steel", overwrite = T) %>%
      add_units("None") %>%
      add_comments("Modeled for all GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_TradedTechnology_bilateral") ->
      L2381.TechShrwt_tra

    L2381.TechCost_tra %>%
      add_title("Technology costs for traded iron and steel", overwrite = T) %>%
      add_units("1975$/kg") %>%
      add_comments("Exogenous cost to reflect shipping + handling of traded commodities") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_TradedTechnology_bilateral") ->
      L2381.TechCost_tra

    L2381.TechCoef_tra %>%
      add_title("Technology input-output coefficients for traded iron and steel", overwrite = T) %>%
      add_units("Unitless IO") %>%
      add_comments("Pass-through; 1 unless some portion is assumed lost/spoiled in shipping") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_TradedTechnology_bilateral") -> L2381.TechCoef_tra

    L2381.Production_tra %>%
      add_title("Technology calibration for traded iron and steel", overwrite = T) %>%
      add_units("Mt") %>%
      add_comments("Regional exports of iron and steel that are traded between GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "LB1092.Tradebalance_iron_steel_Mt_R_Y") -> L2381.Production_tra

    L2381.Supplysector_reg %>%
      add_title("Supplysector info for regional iron and steel", overwrite = T) %>%
      add_units("None") %>%
      add_comments("These sectors are used for sharing between consumption of domestically produced iron and steel versus imports") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_RegionalSector") ->
      L2381.Supplysector_reg

    L2381.SubsectorAll_reg %>%
      add_title("Subsector info for traded iron and steel", overwrite = T) %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_RegionalSubsector") ->
      L2381.SubsectorAll_reg

    L2381.TechShrwt_reg %>%
      add_title("Technology share-weights for traded iron and steel", overwrite = T) %>%
      add_units("None") %>%
      add_comments("Modeled for all GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_RegionalTechnology_bilateral") ->
      L2381.TechShrwt_reg

    L2381.TechCoef_reg %>%
      add_title("Technology input-output coefficients for regional iron and steel", overwrite = T) %>%
      add_units("Unitless IO") %>%
      add_comments("Pass-through; 1 unless some portion is assumed lost/spoiled in shipping") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_RegionalTechnology_bilateral") ->
      L2381.TechCoef_reg

    L2381.Production_reg_imp %>%
      add_title("Technology calibration for regional iron and steel commodities: imports", overwrite = T) %>%
      add_units("Mt") %>%
      add_comments("Consumption of iron and steelthat are traded between GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "LB1092.Tradebalance_iron_steel_Mt_R_Y") ->
      L2381.Production_reg_imp

    L2381.Production_reg_dom %>%
      add_title("Technology calibration for regional iron and steel: consumption of domestic production", overwrite = T) %>%
      add_units("Mt") %>%
      add_comments("Consumption of iron and steel produced within-region") %>%
      add_precursors("common/GCAM_region_names",
                     "LB1092.Tradebalance_iron_steel_Mt_R_Y") ->
      L2381.Production_reg_dom

    # L2381.SubsectorShwtClub_tra %>%
    #   add_title("Subsector shareweights for steel club") %>%
    #   add_units("NA") %>%
    #   add_precursors("common/GCAM_region_names") ->
    #   L2381.SubsectorShwtClub_tra

    return_data(L2381.Supplysector_tra,
                L2381.SectorUseTrialMarket_tra,
                L2381.SubsectorAll_tra,
                # L2381.SubsectorShwtClub_tra,
                L2381.TechShrwt_tra,
                L2381.TechCost_tra,
                L2381.TechCoef_tra,
                L2381.Production_tra,
                L2381.Supplysector_reg,
                L2381.SubsectorAll_reg,
                L2381.TechShrwt_reg,
                L2381.TechCoef_reg,
                L2381.Production_reg_imp,
                L2381.Production_reg_dom)
  } else {
    stop("Unknown command")
  }
}
