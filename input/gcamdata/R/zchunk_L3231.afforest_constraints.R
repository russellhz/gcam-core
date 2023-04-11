# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L3231.afforest_constraints
#'
#' Produce afforestation constraints for specified countries and years
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L3231.aff_mngd_nodes}, \code{L3231.aff_unmngd_nodes}, \code{L3231.aff_constraint}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 20123
module_policy_L3231.afforest_constraints <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_Affor_Constraints",
             "L2231.LN3_MgdAllocation_noncrop",
             "L2231.LN3_UnmgdAllocation"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L3231.affor_mngd_nodes",
             "L3231.affor_unmngd_nodes",
             "L3231.affor_constraint"))
  } else if(command == driver.MAKE) {
    
    all_data <- list(...)[[1]]
    
    # Load required inputs
    A_Affor_Constraints <- get_data(all_data, "policy/A_Affor_Constraints")
    L2231.LN3_MgdAllocation_noncrop <- get_data(all_data, "L2231.LN3_MgdAllocation_noncrop")
    L2231.LN3_UnmgdAllocation <- get_data(all_data, "L2231.LN3_UnmgdAllocation")
    
    L3231.affor_constraint <- A_Affor_Constraints %>% 
      mutate(policyType = "subsidy",
             market = region)
    
    LandNode1_filters <- na.omit(distinct(L3231.affor_constraint, LandNode1_filters))$LandNode1_filters
    LandNode1_filters <- strsplit(LandNode1_filters, ";") %>% unlist()
    
    # Get managed land nodes and add land-constraint-policy name
    L3231.affor_mngd_nodes <- L2231.LN3_MgdAllocation_noncrop %>% 
      filter(region %in% L3231.affor_constraint$region,
             grepl("AllForestLand", LandNode3)) %>% 
      distinct(region, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, LandLeaf) %>% 
      left_join_error_no_match(distinct(L3231.affor_constraint, region, 
                                        land.constraint.policy = policy.portfolio.standard), 
                               by = "region") %>% 
      # Remove any undesired land nodes
      filter(!grepl(LandNode1_filters, LandNode1))
    
    # Now repeat for unmanaged land nodes
    L3231.affor_unmngd_nodes <- L2231.LN3_UnmgdAllocation %>% 
      filter(region %in% L3231.affor_constraint$region,
             grepl("AllForestLand", LandNode3)) %>% 
      distinct(region, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, UnmanagedLandLeaf) %>% 
      left_join_error_no_match(distinct(L3231.affor_constraint, region, 
                                        land.constraint.policy = policy.portfolio.standard), 
                               by = "region") %>% 
      # Remove any undesired land nodes
      filter(!grepl(LandNode1_filters, LandNode1))
    
    
    # Produce outputs
    L3231.affor_constraint %>%
      select(-LandNode1_filters) %>% 
      add_title("Afforestation constraints by region/year", overwrite = T) %>%
      add_units("thous km2") %>%
      add_precursors("policy/A_Affor_Constraints") ->
      L3231.affor_constraint
    
    L3231.affor_mngd_nodes %>%
      add_title("Managed forest to apply afforestation constraint to", overwrite = T) %>%
      add_units("NA") %>%
      add_comments("Removed any nodes listed as nodes_to_remove in INPUT_FILE_TBD") %>%
      add_precursors("L2231.LN3_MgdAllocation_noncrop",
                     "policy/A_Affor_Constraints")  ->
      L3231.affor_mngd_nodes
    
    L3231.affor_unmngd_nodes %>%
      add_title("Unmanaged forest to apply afforestation constraint to", overwrite = T) %>%
      add_units("NA") %>%
      add_comments("Removed any nodes listed as nodes_to_remove in INPUT_FILE_TBD") %>%
      add_precursors("L2231.LN3_UnmgdAllocation",
                     "policy/A_Affor_Constraints")  ->
      L3231.affor_unmngd_nodes
 
    return_data(L3231.affor_mngd_nodes, L3231.affor_unmngd_nodes, L3231.affor_constraint)
  } else {
    stop("Unknown command")
  }
}
