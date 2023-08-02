source("readTechData.R", echo=TRUE)
library(readxl)
library(tidyr)
library(dplyr)
library(quitte)
library(magclass)
library(openxlsx)
library(stringr)
# REF2020 Assumptions --------
power_heat <- readTechData(subtype = "PowerAndHeat")

power_heat_df <- as_tibble(as.data.frame(power_heat)) %>%
  select(-Cell, -Region) %>%
  rename(variable = Data1, Units = Data2, tech_type = Data3) %>%
  # mutate(Year = as.integer(Year)) %>%
  # filter(variable != "Technical lifetime") %>%
  group_by(variable, Units, tech_type) %>%
  mutate(Value = zoo::na.approx(Value)) %>%
  ungroup %>%
  pivot_wider(names_from = Year, values_from = Value) %>%
  relocate(tech_type) %>%
  readr::write_csv("EURef2020_elec_params.csv")


# WEO2022 Assumptions --------
SheetNames <- openxlsx::getSheetNames("3_Power generation technology costs and assumptions_WEO_2022_STEPSandNZE_Scenario.xlsx")
SheetNames <- SheetNames[!SheetNames %in% c("Notes", "Learning rates")]

df_WEO <- tibble()
for (nm in SheetNames){
  df <- readTechData_WEO(nm) %>%
    mutate(subsector = str_to_lower(nm))
  df_WEO <- bind_rows(df_WEO, df)
}

readr::write_csv(df_WEO, "WEO2022_elec_params.csv")
