


### Create Frequency Panel
freqs_panel <- create_freqs_panel(wos_metadata) %>%
  dplyr::filter(year >= 1995 & year <= 2024)

freqs_panel$logged_pop <- log(freqs_panel$population)
freqs_panel$logged_gdp <- log(freqs_panel$gdp)

################################################################################

### Create Synthetic Control Dataset for 'Kenya'
synth1 <- freqs_panel %>% dplyr::filter(ever_ipa == 0 | country == "Kenya")

sc_kenya <- synth1 %>%
  filter(year >= 1995, year <= 2024) %>%
  synthetic_control(
    outcome = frequency_rct,
    unit = country,
    time = year,
    i_unit = "Kenya",
    i_time = 2006) %>%
  generate_predictor(
    frequency_pre = mean(frequency_rct, na.rm = TRUE),
    time_window = 1995:2005) %>%
  generate_weights() %>%
  generate_control()

sc_kenya$.synthetic_control[[1]] %>%
  pivot_longer(
    cols = c(real_y, synth_y),
    names_to = "series",
    values_to = "value") %>%
  ggplot(., aes(x = time_unit, y = value, color = series)) +
  geom_line(size = 1.1) +
  geom_vline(xintercept = 2006, linetype = 2) +
  labs(x = "Year", y = "Outcome", color = "") +
  theme_minimal()

################################################################################

### Create Synthetic Control Dataset for 'South Africa'
synth1 <- freqs_panel %>% dplyr::filter(ever_ipa == 0 | country == "South Africa")

sc_south_africa <- synth1 %>%
  filter(year >= 1995, year <= 2024) %>%
  tidysynth::synthetic_control(
    outcome = frequency_rct,
    unit = country,
    time = year,
    i_unit = "South Africa",
    i_time = 2011) %>%
  tidysynth::generate_predictor(
    frequency_pre = mean(frequency_rct),
    time_window = 1995:2010) %>%
  tidysynth::generate_weights() %>%
  tidysynth::generate_control()

sc_south_africa$.synthetic_control[[1]] %>%
  pivot_longer(
    cols = c(real_y, synth_y),
    names_to = "series",
    values_to = "value") %>%
  ggplot(., aes(x = time_unit, y = value, color = series)) +
  geom_line(size = 1.1) +
  geom_vline(xintercept = 2011, linetype = 2) +
  labs(x = "Year", y = "Outcome", color = "") +
  theme_minimal()
