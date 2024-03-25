# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_iron_steel_trade_bilateral_xml
#'
#' Construct XML data structure for \code{iron_steel_trade_bilateral.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{iron_steel_trade_bilateral.xml}.
module_energy_iron_steel_trade_bilateral_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2381.Supplysector_tra",
             "L2381.SectorUseTrialMarket_tra",
             "L2381.SubsectorAll_tra",
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
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "iron_steel_trade_bilateral.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2381.Supplysector_tra <- get_data(all_data, "L2381.Supplysector_tra")
    L2381.SectorUseTrialMarket_tra <- get_data(all_data, "L2381.SectorUseTrialMarket_tra")
    L2381.SubsectorAll_tra <- get_data(all_data, "L2381.SubsectorAll_tra")
    L2381.TechShrwt_tra <- get_data(all_data, "L2381.TechShrwt_tra")
    L2381.TechCost_tra <- get_data(all_data, "L2381.TechCost_tra")
    L2381.TechCoef_tra <- get_data(all_data, "L2381.TechCoef_tra")
    L2381.Production_tra <- get_data(all_data, "L2381.Production_tra")
    L2381.Supplysector_reg <- get_data(all_data, "L2381.Supplysector_reg")
    L2381.SubsectorAll_reg <- get_data(all_data, "L2381.SubsectorAll_reg")
    L2381.TechShrwt_reg <- get_data(all_data, "L2381.TechShrwt_reg")
    L2381.TechCoef_reg <- get_data(all_data, "L2381.TechCoef_reg")
    L2381.Production_reg_imp <- get_data(all_data, "L2381.Production_reg_imp")
    L2381.Production_reg_dom <- get_data(all_data, "L2381.Production_reg_dom")



    # ===================================================

    # Produce outputs
    create_xml("iron_steel_trade_bilateral.xml") %>%
      add_logit_tables_xml(L2381.Supplysector_tra, "Supplysector") %>%
      add_xml_data(L2381.SectorUseTrialMarket_tra, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L2381.SubsectorAll_tra, "SubsectorAllTo", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L2381.TechShrwt_tra, "TechShrwt") %>%
      add_xml_data(L2381.TechCost_tra, "TechCost") %>%
      add_xml_data(L2381.TechCoef_tra, "TechCoef") %>%
      add_xml_data(L2381.Production_tra, "Production") %>%
      add_logit_tables_xml(L2381.Supplysector_reg, "Supplysector") %>%
      add_logit_tables_xml(L2381.SubsectorAll_reg, "SubsectorAllTo", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L2381.TechShrwt_reg, "TechShrwt") %>%
      add_xml_data(L2381.TechCoef_reg, "TechCoef") %>%
      add_xml_data(L2381.Production_reg_imp, "Production") %>%
      add_xml_data(L2381.Production_reg_dom, "Production") %>%
      add_precursors("L2381.Supplysector_tra",
                     "L2381.SectorUseTrialMarket_tra",
                     "L2381.SubsectorAll_tra",
                     "L2381.TechShrwt_tra",
                     "L2381.TechCost_tra",
                     "L2381.TechCoef_tra",
                     "L2381.Production_tra",
                     "L2381.Supplysector_reg",
                     "L2381.SubsectorAll_reg",
                     "L2381.TechShrwt_reg",
                     "L2381.TechCoef_reg",
                     "L2381.Production_reg_imp",
                     "L2381.Production_reg_dom") ->
      iron_steel_trade_bilateral.xml

    return_data(iron_steel_trade_bilateral.xml)
  } else {
    stop("Unknown command")
  }
}
