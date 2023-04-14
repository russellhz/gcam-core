# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_CTax_xml
#'
#' Construct XML data structure for \code{policy_CTax.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_CTax.xml}.
module_policy_CTax.xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L3222.CTax",
             "L3222.CTax_link_regions"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "policy_CTax.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L3222.CTax <- get_data(all_data, "L3222.CTax")
    L3222.CTax_link_regions <- get_data(all_data, "L3222.CTax_link_regions")
    # ===================================================
    # Need to split L3222.CTax into years with fillout and years without
    L3222.CTax_fillout <- L3222.CTax %>%
      filter(year.fillout == year)

    L3222.CTax_noFillout <- L3222.CTax %>%
      filter(is.na(year.fillout) | year.fillout != year) %>%
      select(-year.fillout)

    # Produce outputs
    create_xml("policy_CTax.xml") %>%
      add_xml_data(L3222.CTax_noFillout, "GHGTax") %>%
      add_xml_data(L3222.CTax_fillout, "GHGTaxFillout") %>%
      add_xml_data(L3222.CTax_link_regions, "GHGConstrMkt") %>%
      add_precursors("L3222.CTax",
                     "L3222.CTax_link_regions") ->
      policy_CTax.xml

    return_data(policy_CTax.xml)
  } else {
    stop("Unknown command")
  }
}
