# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_bld_shell.xml
#'
#' Construct XML data structure for \code{policy_bld_shell.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_inputtax.xml}.
module_policy_bld_shell.xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_building_shell.csv", "policy_bld_shell.xml")
  for(i in 1:length(all_xml_names)){
    if (!grepl(".xml", all_xml_names[i])){
      all_xml_names[i] <- paste0(all_xml_names[i], ".xml")
    }
  }
  if(command == driver.DECLARE_INPUTS) {
    return(c("L344.bld_shell"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L344.bld_shell <- get_data(all_data, "L344.bld_shell")
    # ===================================================

    # Produce outputs

    for (xml_name in all_xml_names){
      L344.bld_shell_tmp <- L344.bld_shell %>%
        filter(xml == xml_name) %>%
        select(-xml)

      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(L344.bld_shell_tmp, "ShellConductance") %>%
               add_precursors("L344.bld_shell")
      )
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
