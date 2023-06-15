# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_FixedOutputTech_xml
#'
#' Construct XML data structure for \code{aeei.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_FixedOutputTech.xml}.
module_policy_FixedOutputTech_xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_FixedOutputTech.csv", "policy_FixedOutputTech.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(c("L305.StubTechFixedOutput",
             "L305.GlbTechFixedOutput"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L305.StubTechFixedOutput <- get_data(all_data, "L305.StubTechFixedOutput")
    L305.GlbTechFixedOutput <- get_data(all_data, "L305.GlbTechFixedOutput")

    # ===================================================

    # Produce outputs
    for (xml_name in all_xml_names){
      L305.StubTechFixedOutput_tmp <- L305.StubTechFixedOutput %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L305.GlbTechFixedOutput_tmp <- L305.GlbTechFixedOutput %>%
        filter(xml == xml_name) %>%
        select(-xml)

      if (nrow(L305.GlbTechFixedOutput_tmp) == 0){
        assign(xml_name,
               create_xml(xml_name) %>%
                 add_xml_data(L305.StubTechFixedOutput_tmp, "StubTechFixOutNoSW") %>%
                 add_precursors("L305.StubTechFixedOutput")
        )
      } else {

        assign(xml_name,
               create_xml(xml_name) %>%
                 add_xml_data(L305.StubTechFixedOutput_tmp, "StubTechFixOutNoSW") %>%
                 add_xml_data(select(L305.GlbTechFixedOutput_tmp,
                                     -minicam.non.energy.input, -input.cost),
                              "GlobalTechShrwt") %>%
                 add_xml_data(select(L305.GlbTechFixedOutput_tmp,
                                     -share.weight),
                              "GlobalTechCost") %>%
                 add_precursors("L305.StubTechFixedOutput",
                                "L305.GlbTechFixedOutput")
        )
      }


    }

    # Need this for loop because having issues with lapply(all_xml_names, get)
    list_of_xmls <- list()
    for(xml_name in all_xml_names){
      list_of_xmls[[xml_name]] <- get(xml_name)
    }
    return_multiple_xmls(list_of_xmls, all_xml_names)

  } else {
    stop("Unknown command")
  }
}
