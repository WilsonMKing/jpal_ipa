################################################################################
######################### J-PAL/IPA Randomization Test #########################
################################################################################

### Create Frequency Panel
freqs_panel <- create_freqs_panel(wos_metadata) %>%
  dplyr::filter(year >= 1995 & year <= 2024) %>%
  dplyr::filter(country %in% dev_world)

### Pull Actual Countries and J-PAL/IPA Years
empirical_jpal <- freqs_panel %>% dplyr::filter(year == 1995) %>% pull(jpal_year)
empirical_ipa <- freqs_panel %>% dplyr::filter(year == 1995) %>% pull(ipa_year)
countries <- freqs_panel %>% dplyr::filter(year == 1995) %>% pull(country)

### Create J-PAL/IPA Variables
create_vars <- function(data){
  
  ### Create Treatment Indicators
  data %<>%
    dplyr::mutate(
      jpal_treated = case_when(
        is.na(jpal_year) ~ 0,
        !is.na(jpal_year) & jpal_year > year ~ 0,
        !is.na(jpal_year) & jpal_year <= year ~ 1),
      ipa_treated = case_when(
        is.na(ipa_year) ~ 0,
        !is.na(ipa_year) & ipa_year > year ~ 0,
        !is.na(ipa_year) & ipa_year <= year ~ 1))
  
  data %<>%
    dplyr::mutate(
      ever_jpal = case_when(
        !is.na(jpal_year) ~ 1,
        is.na(jpal_year) ~ 0),
      ever_ipa = case_when(
        !is.na(ipa_year) ~ 1,
        is.na(ipa_year) ~ 0))
  
}

### Create Storage for Coefficients
placebo_coefficients1 <- rep(NA,1000)
placebo_coefficients2 <- rep(NA,1000)

### Run Monte Carlos
set.seed(910612)
for(i in 1:1000){
  
  ### Remove DHS Variable from Main Dataset
  freqs_panel_no_jpal_ipa <- freqs_panel %>% dplyr::select(-c(jpal_year, ever_jpal, jpal_treated, ipa_year, ever_ipa, ipa_treated))
  
  ### Randomly Assign DHS
  new_jpal_ipa <- data.frame(
    country = countries, 
    ipa_year = sample(empirical_ipa, size = length(countries), replace = TRUE),
    jpal_year = sample(empirical_jpal, size = length(countries), replace = TRUE))
  freqs_panel_jpal_ipa <- left_join(freqs_panel_no_jpal_ipa, new_jpal_ipa, by = "country")
  freqs_panel_jpal_ipa <- create_vars(freqs_panel_jpal_ipa)
  
  ### Run Regression
  reg <- run_jpal_regressions(outcome = "frequency_policy_doc_rct", data = freqs_panel_jpal_ipa, specification = "ols-binary-did-both")

  ### Save Coefficient
  placebo_coefficients1[i] <- reg$coefficients[1]
  placebo_coefficients2[i] <- reg$coefficients[2]
  
}


real_reg1 <- run_jpal_regressions(outcome = "frequency_policy_doc_rct", data = freqs_panel, specification = "ols-binary-did")
real_reg2 <- run_ipa_regressions(outcome = "frequency_policy_doc_rct", data = freqs_panel, specification = "ols-binary-did")


df <- data.frame(
  estimate = c(rep("Jameel Poverty Action Lab", 1000), rep("Innovations for Poverty Action", 1000)),
  vline = c(rep(real_reg1$coefficients[1], 1000), rep(real_reg2$coefficients[1], 1000)),
  coef = c(placebo_coefficients1, placebo_coefficients2))


ggplot(df %>% dplyr::filter(estimate == "Innovations for Poverty Action"), aes(x = coef)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "gray", alpha = 0.6) +
  geom_vline(aes(xintercept = vline), linetype = 2) +
  theme_minimal() +
  labs(x = "Placebo Coefficient",
       y = "Frequency") +
  theme(text = element_text(family = "Times"))

ggsave(filename = paste0(exhibits, "figures/randomization_test_ipa_policy.jpeg"), plot = last_plot(),
       units = "cm", width = 20, height = 10)


ggplot(df %>% dplyr::filter(estimate == "Jameel Poverty Action Lab"), aes(x = coef)) +
  geom_histogram(bins = 30, fill = "orange", color = "gray", alpha = 0.6) +
  geom_vline(aes(xintercept = vline), linetype = 2) +
  theme_minimal() +
  labs(x = "Placebo Coefficient",
       y = "Frequency") +
  theme(text = element_text(family = "Times"))

ggsave(filename = paste0(exhibits, "figures/randomization_test_jpal_policy.jpeg"), plot = last_plot(),
       units = "cm", width = 20, height = 10)

