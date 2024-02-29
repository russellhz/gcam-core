# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_iron_steel_CBAM_xml
#'
#' Construct XML data structure for \code{aeei.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_iron_steel_CBAM_xml}.
module_policy_iron_steel_CBAM_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L3323.iron_steel_CBAM_emiss"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(XML = "iron_steel_CBAM_emiss.xml")
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L3323.iron_steel_CBAM_emiss <- get_data(all_data, "L3323.iron_steel_CBAM_emiss")

    # ===================================================

    # Produce outputs
    create_xml("iron_steel_CBAM_emiss.xml") %>%
      add_xml_data(L3323.iron_steel_CBAM_emiss, "OutputEmissCoeff") %>%
      add_precursors("L3323.iron_steel_CBAM_emiss") ->
      iron_steel_CBAM_emiss.xml

    return_data(iron_steel_CBAM_emiss.xml)

  } else {
    stop("Unknown command")
  }
}
