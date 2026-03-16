################################################################################
############################### Wrangle UCDP Data ##############################
################################################################################

### Load UCDP Data
conflict_data <- readr::read_csv(paste0(data, "UCDP/UCDP_data.csv")) %>%
  dplyr::rename(country = location)

### Clean Up
conflict_data %<>%
  dplyr::select(country, year) %>%
  tidyr::separate_rows(country, sep = ", ") %>%
  distinct(country, year) %>%
  arrange(country, year) %>%
  dplyr::mutate(conflict = 1)

### Rename Countries
conflict_data %<>%
  dplyr::mutate(
    country = case_when(
      country == "Bosnia-Herzegovina" ~ "Bosnia and Herzegovina",
      country == "Brunei" ~ "Brunei Darussalam",
      country == "Cambodia (Kampuchea)" ~ "Cambodia",
      country == "Congo" ~ "Congo, Rep.",
      country == "DR Congo (Zaire)" ~ "Congo, Dem. Rep.",
      country == "Egypt" ~ "Egypt, Arab Rep.",
      country == "Hyderabad" ~ "India",
      country == "Myanmar (Burma)" ~ "Myanmar",
      country == "North Korea" ~ "Korea, Dem. People's Rep.",
      country == "Russia (Soviet Union)" ~ "Russia",
      country == "Serbia (Yugoslavia)" ~ "Serbia",
      country == "South Korea" ~ "Korea, Rep.",
      country == "South Vietnam" ~ "Vietnam",
      country == "South Yemen" ~ "Yemen, Rep.",
      country == "Syria" ~ "Syrian Arab Republic",
      country == "United States of America" ~ "United States",
      country == "Gambia" ~ "Gambia, The",
      country == "Vietnam (North Vietnam)" ~ "Vietnam",
      country == "Yemen (North Yemen)" ~ "Yemen, Rep.",
      country == "Zimbabwe (Rhodesia)" ~ "Zimbabwe",
      country == "Madagascar (Malagasy)" ~ "Madagascar",
      country == "Ivory Coast" ~ "Cote d'Ivoire",
      TRUE ~ country)
  )

conflict_data %<>% unique(.)
