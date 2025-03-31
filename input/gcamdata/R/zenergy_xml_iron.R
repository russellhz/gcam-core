# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_IRON_xml
#'
#' Construct XML data structure for \code{iron.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{iron.xml}. The corresponding file in the
#' original data system was \code{batch_iron_xml.R} (energy XML).
module_energy_IRON_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L23231.Supplysector_iron",
                     "L23231.FinalEnergyKeyword_iron",
                     "L23231.SubsectorLogit_iron",
                     "L23231.SubsectorShrwtFllt_iron",
                     "L23231.SubsectorInterp_iron",
                     "L23231.StubTech_iron",
                     "L23231.GlobalTechShrwt_iron",
                     "L23231.GlobalTechCoef_iron",
                     "L23231.GlobalTechCost_iron",
                     "L23231.GlobalTechTrackCapital_iron",
                     "L23231.GlobalTechSCurve_en",
                     "L23231.GlobalTechProfitShutdown_en",
                     "L23231.StubTechCost_iron",
                     "L23231.StubTechProd_iron",
                     "L23231.StubTechCoef_iron",
                     "L23231.StubTechShrwt_iron")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "iron.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)
    # ===================================================

    # Produce outputs
    create_xml("iron.xml") %>%
      add_logit_tables_xml(L23231.Supplysector_iron, "Supplysector") %>%
      add_xml_data(L23231.FinalEnergyKeyword_iron, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L23231.SubsectorLogit_iron, "SubsectorLogit") %>%
      add_xml_data(L23231.SubsectorShrwtFllt_iron, "SubsectorShrwtFllt") %>%
      add_xml_data(L23231.SubsectorInterp_iron, "SubsectorInterp") %>%
      add_xml_data(L23231.StubTech_iron, "StubTech") %>%
      add_xml_data(L23231.GlobalTechShrwt_iron, "GlobalTechShrwt") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L23231.GlobalTechCoef_iron, "GlobalTechCoef") %>%
      add_xml_data(L23231.GlobalTechTrackCapital_iron, "GlobalTechTrackCapital") %>%
      add_xml_data(L23231.GlobalTechCost_iron, "GlobalTechCost") %>%
      add_xml_data(L23231.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
      add_xml_data(L23231.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
      add_xml_data(L23231.StubTechCost_iron, "StubTechCost") %>%
      add_xml_data(L23231.StubTechProd_iron, "StubTechProd") %>%
      add_xml_data(L23231.StubTechCoef_iron, "StubTechCoef") %>%
      add_xml_data(L23231.StubTechShrwt_iron, "StubTechShrwt") %>%
      add_precursors("L23231.Supplysector_iron", "L23231.FinalEnergyKeyword_iron", "L23231.SubsectorLogit_iron",
                     "L23231.SubsectorShrwtFllt_iron",
                     "L23231.SubsectorInterp_iron",
                     "L23231.StubTech_iron",
                     "L23231.GlobalTechShrwt_iron", "L23231.GlobalTechCoef_iron", "L23231.GlobalTechCost_iron",
                     "L23231.GlobalTechSCurve_en",
                     "L23231.GlobalTechProfitShutdown_en", "L23231.StubTechProd_iron",
                     "L23231.StubTechCoef_iron",
                     "L23231.StubTechCost_iron",
                     "L23231.GlobalTechTrackCapital_iron") ->
      iron.xml
    return_data(iron.xml)
  } else {
    stop("Unknown command")
  }
}

