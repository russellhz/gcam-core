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
#' the generated outputs: \code{L3222.CTax}, \code{L3222.CTax_link_regions}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L3222.CTax <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_CTax",
             FILE = "policy/A_CTax_Region"
    ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L3222.CTax",
             "L3222.CTax_link_regions"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_CTax <- get_data(all_data, "policy/A_CTax")
    A_CTax_Region <- get_data(all_data, "policy/A_CTax_Region")

    # Write tax to correct regions
    # When a tax applies to more than one region, we don't need to write it
    # to all regions, can just pass the ghg policy forward

    # First if we have any multi-region taxes, get just the first region from each
    if(nrow(A_CTax_Region) > 0){
      region_to_map_market <- A_CTax_Region %>%
        group_by(market, ghgpolicy) %>%
        filter(row_number() == 1) %>%
        ungroup
    } else {
      region_to_map_market <- tibble(market = character(),
                                     ghgpolicy = character(),
                                     region = character())
    }

    # Now join in the first region from multi-region taxes
    # If single region tax (region is NA), set region to market
    tax_regions <- A_CTax %>%
      select(market, ghgpolicy) %>%
      left_join(region_to_map_market, by = c("market", "ghgpolicy")) %>%
      mutate(region = if_else(is.na(region), market, region))


    L3222.CTax <- A_CTax %>%
      gather_years() %>%
      filter(!is.na(value)) %>%
      left_join_error_no_match(tax_regions, by = c("market", "ghgpolicy")) %>%
      rename(fixedTax = value)

    L3222.CTax_link_regions <- A_CTax_Region %>%
      left_join(L3222.CTax, by = c("region", "market", "ghgpolicy")) %>%
      select(xml, region, market, ghgpolicy) %>%
      anti_join(L3222.CTax, by = c("region", "market", "ghgpolicy"))

    # Produce outputs
    L3222.CTax %>%
      add_title("Carbon taxes", overwrite = T) %>%
      add_units("$1975/MTC") %>%
      add_precursors("policy/A_CTax",
                     "policy/A_CTax_Region") ->
      L3222.CTax

    L3222.CTax_link_regions %>%
      add_title("Region linked to multi-region carbon taxes", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_CTax",
                     "policy/A_CTax_Region") ->
      L3222.CTax_link_regions

    return_data(L3222.CTax, L3222.CTax_link_regions)
  } else {
    stop("Unknown command")
  }
}

