library(readxl)
library(magrittr)
library(gcamdata)

yy = c(1960,1975,1990,2005,2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2065,2070,2075,2080,2085,2090,2095,2100)

## -- population
population = read_excel("../harmonization/gdp_pop_iam_compact_20230625.xlsx", sheet = "Population") %>%
  dplyr::select(-c('VARIABLE','UNIT')) %>%
  dplyr::rename('iso' = 'REGION') %>%
  dplyr::mutate(iso = tolower(iso)) %>%
  dplyr::left_join(read.csv("gcamdata/inst/extdata/common/iso_GCAM_regID.csv", skip = 6), by = 'iso')
population = tidyr::pivot_longer(population, cols = 2:152) %>%
  dplyr::rename('year' = 'name') %>%
  dplyr::group_by(region_GCAM3,year) %>%
  dplyr::summarise(val = round(sum(value)/1000)) %>%    # unit: in thousands
  dplyr::filter(year %in% yy)
population = tidyr::pivot_wider(population, names_from = 'year', values_from = 'val')
write.csv(population, file = "gcamdata/inst/extdata/socioeconomics/GCAM3_population_iamcompact.csv")


## -- gdp
gdp = read_excel("../harmonization/gdp_pop_iam_compact_20230625.xlsx", sheet = "GDP PPP") %>%
  dplyr::select(-c('VARIABLE','UNIT')) %>%
  dplyr::rename('iso' = 'REGION') %>%
  dplyr::mutate(iso = tolower(iso)) %>%
  dplyr::left_join(read.csv("gcamdata/inst/extdata/common/iso_GCAM_regID.csv", skip = 6), by = 'iso') %>%
  dplyr::filter(!is.na(region_GCAM3))    # erase UVK because it did not match any country
gdp = tidyr::pivot_longer(gdp, cols = 2:102) %>%
  dplyr::rename('year' = 'name') %>%
  dplyr::group_by(region_GCAM3,year) %>%
  dplyr::summarise(val = round(sum(value, na.rm = TRUE)*gdp_deflator(1990,2017)/1e6)) %>%    # unit: million 1990 USD
  dplyr::filter(year %in% yy)
gdp = tidyr::pivot_wider(gdp, names_from = 'year', values_from = 'val')
write.csv(gdp, file = "gcamdata/inst/extdata/socioeconomics/GCAM3_GDP_iamcompact.csv")
