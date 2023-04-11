# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_aeei_xml
#'
#' Construct XML data structure for \code{aeei.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{aeei.xml}.
module_policy_aeei_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L326.aeei"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "policy_aeei.xml"))
  } else if(command == driver.MAKE) {
    
    all_data <- list(...)[[1]]
    
    # Load required inputs
    L326.aeei <- get_data(all_data, "L326.aeei")
    # ===================================================
    
    # Produce outputs
    create_xml("policy_aeei.xml") %>%
      add_xml_data(L326.aeei, "aeei") %>%
      add_precursors("L326.aeei") ->
      policy_aeei.xml
    
    return_data(policy_aeei.xml)
  } else {
    stop("Unknown command")
  }
}
