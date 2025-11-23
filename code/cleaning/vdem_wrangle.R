################################################################################
######################## Data Wrangling: VDEM Indicators #######################
################################################################################

### Load V-DEM Package if Necessary
devtools::install_github("vdeminstitute/vdemdata")

### Read in Data
vdem <- vdemdata::vdem

### Select Variables
vdem %<>% dplyr::select(country_name, country_text_id, year, v2x_regime)

### Rename Variables
vdem %<>%
  dplyr::rename(
    country = country_name,
    country.code = country_text_id,
    year = year,
    regime = v2x_regime)

### Rename Codes
vdem %<>%
  dplyr::mutate(
    regime = case_when(
      regime == 0 ~ "Closed Autocracy",
      regime == 1 ~ "Electoral Autocracy",
      regime == 2 ~ "Electoral Democracy",
      regime == 3 ~ "Liberal Democracy"))
