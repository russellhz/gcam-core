# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_STEEL_xml
#'
#' Construct XML data structure for \code{steel.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{steel.xml}. The corresponding file in the
#' original data system was \code{batch_steel_xml.R} (energy XML).
module_energy_STEEL_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L23232.Supplysector_steel",
                     "L23232.FinalEnergyKeyword_steel",
                     "L23232.SubsectorLogit_steel",
                     "L23232.SubsectorShrwtFllt_steel",
                     "L23232.SubsectorInterp_steel",
                     "L23232.StubTech_steel",
                     "L23232.GlobalTechShrwt_steel",
                     "L23232.GlobalTechCoef_steel",
                     "L23232.GlobalTechCost_steel",
                     "L23232.GlobalTechTrackCapital_steel",
                     "L23232.GlobalTechSCurve_en",
                     "L23232.GlobalTechProfitShutdown_en",
                     "L23232.StubTechProd_steel",
                     "L23232.StubTechShrwt_steel")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "steel.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)
    # ===================================================

    # Produce outputs
    create_xml("steel.xml") %>%
      add_logit_tables_xml(L23232.Supplysector_steel, "Supplysector") %>%
      add_xml_data(L23232.FinalEnergyKeyword_steel, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L23232.SubsectorLogit_steel, "SubsectorLogit") %>%
      add_xml_data(L23232.SubsectorShrwtFllt_steel, "SubsectorShrwtFllt") %>%
      add_xml_data(L23232.SubsectorInterp_steel, "SubsectorInterp") %>%
      add_xml_data(L23232.StubTech_steel, "StubTech") %>%
      add_xml_data(L23232.GlobalTechShrwt_steel, "GlobalTechShrwt") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L23232.GlobalTechCoef_steel, "GlobalTechCoef") %>%
      add_xml_data(L23232.GlobalTechTrackCapital_steel, "GlobalTechTrackCapital") %>%
      add_xml_data(L23232.GlobalTechCost_steel, "GlobalTechCost") %>%
      add_xml_data(L23232.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
      add_xml_data(L23232.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
      add_xml_data(L23232.StubTechProd_steel, "StubTechProd") %>%
      add_xml_data(L23232.StubTechShrwt_steel, "StubTechShrwt") %>%
      add_precursors("L23232.Supplysector_steel", "L23232.FinalEnergyKeyword_steel", "L23232.SubsectorLogit_steel",
                     "L23232.SubsectorShrwtFllt_steel",
                     "L23232.SubsectorInterp_steel",
                     "L23232.StubTech_steel",
                     "L23232.GlobalTechShrwt_steel", "L23232.GlobalTechCoef_steel", "L23232.GlobalTechCost_steel",
                     "L23232.GlobalTechSCurve_en",
                     "L23232.GlobalTechProfitShutdown_en", "L23232.StubTechProd_steel",
                     "L23232.GlobalTechTrackCapital_steel") ->
      steel.xml
    return_data(steel.xml)
  } else {
    stop("Unknown command")
  }
}

