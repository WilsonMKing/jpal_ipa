

### Create Analysis Dataset
freqs_panel <- create_freqs_panel(wos_metadata) %>%
  dplyr::filter(year >= 1995 & year <= 2024)

### Create Logged Population/GDP Variables
freqs_panel$logged_pop <- log(freqs_panel$population)
freqs_panel$logged_gdp <- log(freqs_panel$gdp)

### Create Binary Treatment Indicators
freqs_panel$at_least_one_experiment <- ifelse(freqs_panel$frequency_rct > 0, 1, 0)

didimputation::did_imputation(
  data = freqs_panel %>% dplyr::filter(ever_jpal == 1),
  yname = "frequency_rct",
  gname = "jpal_year",
  tname = "year",
  idname = "country_id")

didimputation::did_imputation(
  data = freqs_panel %>% dplyr::filter(ever_ipa == 1),
  yname = "frequency_rct",
  gname = "ipa_year",
  tname = "year",
  idname = "country_id")

didimputation::did_imputation(
  data = freqs_panel %>% dplyr::filter(ever_jpal == 1),
  yname = "at_least_one_experiment",
  gname = "jpal_year",
  tname = "year",
  idname = "country_id")

didimputation::did_imputation(
  data = freqs_panel %>% dplyr::filter(ever_ipa == 1),
  yname = "at_least_one_experiment",
  gname = "ipa_year",
  tname = "year",
  idname = "country_id")
