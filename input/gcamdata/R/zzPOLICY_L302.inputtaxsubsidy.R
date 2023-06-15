# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_302.inputtaxsubsidy
#'
#' Produce fuel standards coefficients
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L302.InputTax}, \code{L302.InputSubsidy}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_302.inputtaxsubsidy <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_InputTaxesSubsidies"
    ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L302.InputTax"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_InputTaxesSubsidies <- get_data(all_data, "policy/A_InputTaxesSubsidies")

    # Convert to long
    L302.InputTax <- A_InputTaxesSubsidies %>%
      gather_years(value_col = "input.cost") %>%
      complete(nesting(xml, region, supplysector, subsector, stub.technology, minicam.non.energy.input),
               year = seq(min(year), max(year), 5)) %>%
      group_by(xml, region, supplysector, subsector, stub.technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, input.cost)) %>%
      ungroup

    # Produce outputs
    L302.InputTax %>%
      add_title("Input taxes for specific techs", overwrite = T) %>%
      add_units("$1975 (usually per GJ") %>%
      add_precursors("policy/A_InputTaxesSubsidies") ->
      L302.InputTax

    return_data(L302.InputTax)
  } else {
    stop("Unknown command")
  }
}

