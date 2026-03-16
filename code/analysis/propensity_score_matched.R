################################################################################
########################## Alternative Control Groups ##########################
################################################################################


### Create Panel of Frequencies
freqs_panel <- create_freqs_panel(wos_metadata) %>%
  dplyr::filter(year >= 1995 & year <= 2024) %>%
  dplyr::mutate(
    jpal_dummy = case_when(
      jpal_year == year ~ 1,
      TRUE ~ 0),
    ipa_dummy = case_when(
      ipa_year == year ~ 1,
      TRUE ~ 0),
    logged_gdp = log(gdp),
    logged_pop = log(population),
    electoral_democracy = ifelse(regime == "Electoral Democracy", 1, 0),
    electoral_autocracy = ifelse(regime == "Electoral Autocracy", 1, 0),
    liberal_democracy = ifelse(regime == "Liberal Democracy", 1, 0),
    conflict = factor(conflict))

### Set Logit Formula
logit_form <- jpal_dummy ~ logged_pop + logged_gdp + samerica + asia + africa + europe + electoral_democracy + electoral_autocracy + liberal_democracy + conflict

### Estimate Logit Model
logit_model <- glm(formula = logit_form, family = binomial(link = "logit"), data = freqs_panel)

### Compute Fitted Values
freqs_panel %<>% dplyr::mutate(fitted_prob = predict(logit_model, newdata = ., type = "response"))

### Set Dataframes with Fitted Values for J-PAL and IPA Countries
jpal_treated_probs <- freqs_panel %>% dplyr::filter(jpal_dummy == 1) %>% dplyr::select(c(country, year, fitted_prob))
ipa_treated_probs <- freqs_panel %>% dplyr::filter(ipa_dummy == 1) %>% dplyr::select(c(country, year, fitted_prob))

### Create Storage
jpal_matched_controls <- list()
ipa_matched_controls <- list()

### Loop Through J-PAL Countries
for(c in seq_along(jpal_treated_probs$country)){
  
  country <- freqs_panel %>% 
    dplyr::filter(year == as.numeric(jpal_treated_probs[c,2]) & ever_jpal == 0) %>% # Restrict to Same Year
    dplyr::select(c(country, fitted_prob)) %>%
    dplyr::mutate(delta_prob = abs(fitted_prob - as.numeric(jpal_treated_probs[c,3]))) %>% # Compute Absolute Value of Difference in Fitted Values
    dplyr::arrange(delta_prob) %>% # Arrange by Absolute Value of Difference in Fitted Values
    dplyr::slice(1:3) %>% # Choose Closest Two Countries
    pull(country) %>%  # Pull Country Names
    as.character()
  
  # Save
  jpal_matched_controls[[c]] <- country
  
}

### Loop Through IPA Countries
for(c in seq_along(ipa_treated_probs$country)){
  
  country <- freqs_panel %>% 
    dplyr::filter(year == as.numeric(ipa_treated_probs[c,2]) & ever_ipa == 0) %>% # Restrict to Same Year
    dplyr::select(c(country, fitted_prob)) %>%
    dplyr::mutate(delta_prob = abs(fitted_prob - as.numeric(ipa_treated_probs[c,3]))) %>% # Compute Absolute Value of Difference in Fitted Values
    dplyr::arrange(delta_prob) %>% # Arrange by Absolute Value of Difference in Fitted Values
    dplyr::slice(1:3) %>% # Choose Closest Two Countries
    pull(country) %>% # Pull Country Names
    as.character()
  
  ipa_matched_controls[[c]] <- country
  
}

################################################################################

### Re-Create Frequencies Panel
freqs_panel <- create_freqs_panel(wos_metadata) %>%
  dplyr::filter(year >= 1995 & year <= 2024) 

### Create List of Low Population Countries
low_pop_countries <- freqs_panel %>%
  group_by(country) %>%
  dplyr::summarize(min = min(population)) %>%
  dplyr::filter(min <= 2000000) %>%
  pull(country)

run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel %>% dplyr::filter(!(country %in% low_pop_countries)), controls = FALSE)
run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel %>% dplyr::filter(country %in% dev_world), controls = FALSE)

run_ipa_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel %>% dplyr::filter(!(country %in% low_pop_countries)), controls = FALSE)
run_ipa_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel %>% dplyr::filter(country %in% dev_world), controls = FALSE)

run_ipa_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel %>% dplyr::filter(country %in% unlist(ipa_matched_controls) | ever_ipa == 1), controls = FALSE)
run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel %>% dplyr::filter(country %in% unlist(jpal_matched_controls) | ever_jpal == 1), controls = FALSE)



twfe_ols[["ipa"]] <- run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did")
