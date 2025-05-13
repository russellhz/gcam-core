# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_iron_steel_new_xml
#'
#' Construct XML data structure for \code{iron_steel_new.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{iron_steel_new.xml}. The corresponding file in the
#' original data system was \code{batch_iron_steel_new_xml.R} (energy XML).
module_energy_iron_steel_new_xml <- function(command, ...) {
  # Some techs aren't split between iron and steel
  # for these techs, we want to keep the old data, but remove iron and steel techs
  FULL_IRON_STEEL_TECHS <- c("L2323.GlobalTechCoef_iron_steel",
                             "L2323.GlobalTechCost_iron_steel",
                             "L2323.GlobalTechTrackCapital_iron_steel",
                             "L2323.GlobalTechCapture_iron_steel",
                             "L2323.GlobalTechShutdown_en",
                             "L2323.GlobalTechSCurve_en",
                             "L2323.GlobalTechLifetime_en",
                             "L2323.GlobalTechProfitShutdown_en",
                             "L2323.StubTechCoef_iron_steel",
                             "L2323.StubTechCost_iron_steel")

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
                     "L23231.GlobalTechCapture_iron",
                     "L23231.StubTechCost_iron",
                     "L23231.StubTechProd_iron",
                     "L23231.StubTechCoef_iron",
                     "L23231.StubTechCoef_steel",
                     "L23231.StubTechShrwt_iron",

                     "L23232.Supplysector_steel",
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

                     "L2323.Supplysector_iron_steel",
                     "L2323.FinalEnergyKeyword_iron_steel",
                     "L2323.SubsectorLogit_iron_steel",
                     "L2323.SubsectorShrwtFllt_iron_steel",
                     "L2323.SubsectorInterp_iron_steel",
                     "L2323.StubTech_iron_steel",
                     "L2323.GlobalTechShrwt_iron_steel",
                     "L23233.GlobalTechCoef_iron_steel_new",
                     "L2323.StubTechShrwt_iron_steel",
                     "L2323.StubTechProd_iron_steel",
                     "L2323.PerCapitaBased_iron_steel",
                     "L2323.BaseService_iron_steel",
                     "L2323.PriceElasticity_iron_steel",

                     "L2323.GlobalTechCoef_iron_steel",
                     "L2323.GlobalTechCost_iron_steel",
                     "L2323.GlobalTechTrackCapital_iron_steel",
                     "L2323.GlobalTechCapture_iron_steel",
                     "L2323.GlobalTechShutdown_en",
                     "L2323.GlobalTechSCurve_en",
                     "L2323.GlobalTechLifetime_en",
                     "L2323.GlobalTechProfitShutdown_en",
                     "L2323.StubTechCoef_iron_steel",
                     "L2323.StubTechCost_iron_steel",

                     FULL_IRON_STEEL_TECHS)
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "iron_steel_new.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = T)

    SPLIT_TECHS <- L23233.GlobalTechCoef_iron_steel_new %>% distinct(sector.name) %>%  pull()

    # function to remove SPLIT_TECHS from FULL_IRON_STEEL_TECHS objects
    filter_sector_names_str <- function(tbl_name_str) {
      # Get the actual object
      tbl <- get(tbl_name_str, envir = parent.frame())

      if(is.null(tbl)){
        assign(tbl_name_str, tbl, envir = parent.frame())
        return(invisible(NULL))
      }

      # Check for valid column to filter on
      filter_col <- intersect(c("sector.name", "supplysector"), colnames(tbl))

      if (length(filter_col) == 1) {
        # Filter the table
        tbl_filtered <- tbl %>%
          dplyr::filter(!.data[[filter_col]] %in% SPLIT_TECHS)

        # Assign back to original name in calling environment
        assign(tbl_name_str, tbl_filtered, envir = parent.frame())
      } else {
        warning(paste("Neither 'sector.name' nor 'supplysector' found in", tbl_name_str))
      }
    }

    for (name in FULL_IRON_STEEL_TECHS) {
      filter_sector_names_str(name)
    }

    # ===================================================

    # Produce outputs
    create_xml("iron_steel_new.xml") %>%
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
      add_xml_data(L23231.GlobalTechCapture_iron, "GlobalTechCapture") %>%
      add_xml_data(L23231.StubTechCost_iron, "StubTechCost") %>%
      add_xml_data(L23231.StubTechProd_iron, "StubTechProd") %>%
      add_xml_data(L23231.StubTechCoef_iron, "StubTechCoef") %>%
      add_xml_data(L23231.StubTechCoef_steel, "StubTechCoef") %>%
      add_xml_data(L23231.StubTechShrwt_iron, "StubTechShrwt") %>%

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

      add_logit_tables_xml(L2323.Supplysector_iron_steel, "Supplysector") %>%
      add_xml_data(L2323.FinalEnergyKeyword_iron_steel, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2323.SubsectorLogit_iron_steel, "SubsectorLogit") %>%
      add_xml_data(L2323.SubsectorShrwtFllt_iron_steel, "SubsectorShrwtFllt") %>%
      add_xml_data(L2323.SubsectorInterp_iron_steel, "SubsectorInterp") %>%
      add_xml_data(L2323.StubTech_iron_steel, "StubTech") %>%
      add_xml_data(L2323.GlobalTechShrwt_iron_steel, "GlobalTechShrwt") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L23233.GlobalTechCoef_iron_steel_new, "GlobalTechCoef") %>%
      add_xml_data(L2323.StubTechProd_iron_steel, "StubTechProd") %>%
      add_xml_data(L2323.StubTechShrwt_iron_steel, "StubTechShrwt") %>%
      add_xml_data(L2323.PerCapitaBased_iron_steel, "PerCapitaBased") %>%
      add_xml_data(L2323.BaseService_iron_steel, "BaseService") %>%
      add_xml_data(L2323.PriceElasticity_iron_steel, "PriceElasticity") %>%

      # add_node_equiv_xml("input") %>%
      add_xml_data(L2323.GlobalTechCoef_iron_steel, "GlobalTechCoef") %>%
      add_xml_data(L2323.GlobalTechTrackCapital_iron_steel, "GlobalTechTrackCapital") %>%
      add_xml_data(L2323.GlobalTechCost_iron_steel, "GlobalTechCost") %>%
      add_xml_data(L2323.GlobalTechCapture_iron_steel, "GlobalTechCapture") %>%
      add_xml_data(L2323.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
      add_xml_data(L2323.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
      # Don't want any stub tech costs
      add_xml_data(L2323.StubTechCost_iron_steel, "StubTechCost") %>%
      add_xml_data(L2323.StubTechCoef_iron_steel, "StubTechCoef") %>%

      add_precursors(MODULE_INPUTS) ->
      iron_steel_new.xml
    return_data(iron_steel_new.xml)
  } else {
    stop("Unknown command")
  }
}

