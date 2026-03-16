################################################################################
#################### Wrangle Overton Policy Documents Data #####################
################################################################################

### Load Overton Data
overton_df <- readr::read_csv(paste0(data, "overton/overton_rcts.csv")) %>%
  dplyr::rename(country = `Source country`) %>%
  dplyr::filter(!(country %in% c("IGO", "Hong Kong", "Taiwan", "EU")))

### Rename Countries
overton_df %<>%
  dplyr::mutate(country = case_when(
    country == "UK" ~ "United Kingdom",
    country == "USA" ~ "United States",
    country == "Laos" ~ "Lao PDR",
    country == "Iran" ~ "Iran, Islamic Rep.",
    country == "Bahamas" ~ "Bahamas, The",
    country == "Kyrgyzstan" ~ "Kyrgyz Republic",
    country == "Slovakia" ~ "Slovak Republic",
    country == "Syria" ~ "Syrian Arab Republic",
    country == "Turkey" ~ "Turkiye",
    country == "South Korea" ~ "Korea, Rep.",
    country == "Timor Leste" ~ "Timor-Leste",
    country == "Russia" ~ "Russian Federation",
    country == "Cape Verde" ~ "Cabo Verde",
    country == "Egypt" ~ "Egypt, Arab Rep.",
    country == "Bosnia" ~ "Bosnia and Herzegovina",
    country == "Brunei" ~ "Brunei Darussalam",
    country == "Czech Republic" ~ "Czechia",
    TRUE ~ country))

### Create Year Variable
overton_df %<>%
  dplyr::mutate(year = as.numeric(format(as.Date(Published_on), "%Y"))) %>% 
  dplyr::filter(!is.na(year) & !is.na(country))

### Create Panel of Policy Document References to RCTs
overton_panel <- overton_df %>%
  group_by(country, year) %>%
  dplyr::summarize(frequency_policy_doc_rct = n()) %>%
  ungroup()

################################################################################

### Load Other Overton Data
overton_1 <- readr::read_csv(paste0(data, "overton/overton_1.csv")) %>%
  dplyr::rename(country = `Source country`) %>%
  dplyr::filter(!(country %in% c("IGO", "Hong Kong", "Taiwan", "EU")))

overton_2 <- readr::read_csv(paste0(data, "overton/overton_2.csv")) %>%
  dplyr::rename(country = `Source country`) %>%
  dplyr::filter(!(country %in% c("IGO", "Hong Kong", "Taiwan", "EU")))

### Row Bind Overton Datasets
overton_econ <- rbind(overton_1, overton_2)

### Rename Countries
overton_econ %<>%
  dplyr::mutate(country = case_when(
    country == "UK" ~ "United Kingdom",
    country == "USA" ~ "United States",
    country == "Laos" ~ "Lao PDR",
    country == "Iran" ~ "Iran, Islamic Rep.",
    country == "Bahamas" ~ "Bahamas, The",
    country == "Kyrgyzstan" ~ "Kyrgyz Republic",
    country == "Slovakia" ~ "Slovak Republic",
    country == "Syria" ~ "Syrian Arab Republic",
    country == "Turkey" ~ "Turkiye",
    country == "South Korea" ~ "Korea, Rep.",
    country == "Timor Leste" ~ "Timor-Leste",
    country == "Russia" ~ "Russian Federation",
    country == "Cape Verde" ~ "Cabo Verde",
    country == "Egypt" ~ "Egypt, Arab Rep.",
    country == "Bosnia" ~ "Bosnia and Herzegovina",
    country == "Brunei" ~ "Brunei Darussalam",
    country == "Czech Republic" ~ "Czechia",
    TRUE ~ country))

### Create Year Variable
overton_econ %<>%
  dplyr::mutate(year = as.numeric(format(as.Date(Published_on), "%Y"))) %>% 
  dplyr::filter(!is.na(year) & !is.na(country))

### Create Panel of Policy Document References to RCTs
overton_econ_panel <- overton_econ %>%
  group_by(country, year) %>%
  dplyr::summarize(frequency_policy_doc_econ = n()) %>%
  ungroup()

### Create Final Dataset
overton_panel <- full_join(overton_panel, overton_econ_panel, by = c("country", "year"))

