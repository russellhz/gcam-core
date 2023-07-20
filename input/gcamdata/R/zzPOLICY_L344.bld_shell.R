# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L344.bld_shell
#'
#' Produce changes to building shell conductance
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L344.bld_shell}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 20123
module_policy_L344.bld_shell <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_building_shell",
             "L244.ShellConductance_bld"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L344.bld_shell"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_building_shell <- get_data(all_data, "policy/A_building_shell") %>%
      mutate(xml = if_else(grepl(".xml", xml), xml, paste0(xml, ".xml")))

    L244.ShellConductance_bld <- get_data(all_data, "L244.ShellConductance_bld")

    # Convert to long format and interpolate any missing years
    L344.bld_shell_overwrite <- A_building_shell %>%
      gather_years() %>%
      group_by(xml, region, gcam.consumer) %>%
      # Interpolates between min and max years in A_aeei
      complete(nesting(xml, region, gcam.consumer), year = seq(min(year), max(year), 5)) %>%
      # If group only has one, approx_fun doesn't work, so we use this workaround
      mutate(value_NA = as.numeric(approx_fun(year, value))) %>%
      ungroup %>%
      mutate(shell.conductance = if_else(!is.na(value_NA), value_NA, value)) %>%
      select(-value_NA, -value)

    # Now replace shell.conductance in L244.ShellConductance_bld
    L344.bld_shell <- L244.ShellConductance_bld %>%
      semi_join(L344.bld_shell_overwrite, by = c("region", "gcam.consumer")) %>%
      left_join(L344.bld_shell_overwrite, by = c("region", "gcam.consumer", "year")) %>%
      mutate(shell.conductance = if_else(is.na(shell.conductance.y), shell.conductance.x, shell.conductance.y)) %>%
      select(-shell.conductance.x, -shell.conductance.y, -xml) %>%
      # Make sure all years have xml name
      left_join_error_no_match(distinct(select(L344.bld_shell_overwrite, region, gcam.consumer, xml)), by = c("region", "gcam.consumer"))


    # Produce outputs
    L344.bld_shell %>%
      add_title("Building shell conductance overwrite", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_building_shell",
                     "L244.ShellConductance_bld") ->
      L344.bld_shell

    return_data(L344.bld_shell)
  } else {
    stop("Unknown command")
  }
}
