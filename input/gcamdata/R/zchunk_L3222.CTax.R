# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L3222.CTax
#'
#' Produce custom carbon cap targets and markets
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L3222.CTax}, \code{L3222.CTax_Link}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L3222.CTax <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_CTax",
             FILE = "policy/A_CTax_Link")
           )
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L3222.CTax",
             "L3222.CTax_Region_Link",
             "L3222.CTax_GHG_Link"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_CTax <- get_data(all_data, "policy/A_CTax")
    A_CTax_Link <- get_data(all_data, "policy/A_CTax_Link")

    # Write tax to correct regions
    L3222.CTax <- A_CTax %>%
      gather_years(value_col = "fixedTax") %>%
      filter(!is.na(fixedTax)) %>%
      select(-link.type)

    # If any linked regions, pull out here
    # Assumes linked regions have no taxes listed
    L3222.CTax_Region_Link <- A_CTax %>%
      anti_join(L3222.CTax, by = c("xml", "market", "region", "ghgpolicy", "year.fillout")) %>%
      select(xml, LEVEL2_DATA_NAMES[["GHGConstrMkt"]])

    # Get ghg link for each
    L3222.CTax_GHG_Link <- A_CTax %>%
      distinct(link.type, market, region, ghgpolicy) %>%
      left_join(A_CTax_Link, by = "link.type") %>%
      rename(linked.policy = ghgpolicy) %>%
      select(-link.type)

    # Produce outputs
    L3222.CTax %>%
      add_title("Carbon taxes", overwrite = T) %>%
      add_units("$1975/MTC") %>%
      add_precursors("policy/A_CTax") ->
      L3222.CTax

    L3222.CTax_GHG_Link %>%
      add_title("GHG links for carbon taxes", overwrite = T) %>%
      add_units("Various") %>%
      add_precursors("policy/A_CTax",
                     "policy/A_CTax_Link") ->
      L3222.CTax_GHG_Link

    L3222.CTax_Region_Link %>%
      add_title("Region links for carbon taxes", overwrite = T) %>%
      add_units("Various") %>%
      add_precursors("policy/A_CTax") ->
      L3222.CTax_Region_Link

    return_data(L3222.CTax, L3222.CTax_GHG_Link, L3222.CTax_Region_Link)
  } else {
    stop("Unknown command")
  }
}

