################################################################################
######################## Country Leave-One-Out Exercises #######################
################################################################################

### Create Analysis Dataset
freqs_panel <- create_freqs_panel(wos_metadata) %>%
  dplyr::filter(year >= 1995 & year <= 2024) %>%
  dplyr::filter(country %in% dev_world)

### Create Storage for J-PAL Coefficients/SEs
jpal_coefs <- list()
jpal_ses <- list()

### Create Storage for IPA Coefficients/SEs
ipa_coefs <- list()
ipa_ses <- list()

### Create Vectors of Treated Countries
jpal_treated_countries <- unique(freqs_panel$country[freqs_panel$jpal_treated == 1])
ipa_treated_countries <- unique(freqs_panel$country[freqs_panel$ipa_treated == 1])

### Loop Through J-PAL Countries
for(c in seq_along(jpal_treated_countries)){
  temp_df <- freqs_panel %>% dplyr::filter(country != jpal_treated_countries[c])
  reg <- run_jpal_regressions(outcome = "frequency_policy_doc_rct", specification = "ols-binary-did", data = temp_df)
  jpal_coefs[c] <- reg$coefficients[1]
  jpal_ses[c] <- reg$cse[1]
}

### Loop Through IPA Countries
for(c in seq_along(ipa_treated_countries)){
  temp_df <- freqs_panel %>% dplyr::filter(country != ipa_treated_countries[c])
  reg <- run_ipa_regressions(outcome = "frequency_policy_doc_rct", specification = "ols-binary-did", data = temp_df)
  ipa_coefs[c] <- reg$coefficients[1]
  ipa_ses[c] <- reg$cse[1]
}

### Create Dataframe of IPA Results
ipa_results_df <- data.frame(
  org = "Innovations for Poverty Action",
  country = ipa_treated_countries,
  coef = unlist(ipa_coefs),
  se = unlist(ipa_ses))

### Create Dataframe of J-PAL Results
jpal_results_df <- data.frame(
  org = "Jameel Poverty Action Lab",
  country = jpal_treated_countries,
  coef = unlist(jpal_coefs),
  se = unlist(jpal_ses))

### Merge Results
results_df <- rbind(ipa_results_df, jpal_results_df)

reg1 <- run_ipa_regressions(outcome = "frequency_policy_doc_rct", specification = "ols-binary-did")
reg2 <- run_jpal_regressions(outcome = "frequency_policy_doc_rct", specification = "ols-binary-did")

### Plot Results
plot1 <- ggplot(jpal_results_df, 
                aes(y = country, x = coef, xmin = coef - 1.96*se, xmax = coef + 1.96*se)) +
  geom_rect(
    xmin = reg2$coefficients[1] - 1.96*reg2$cse[1],
    xmax = reg2$coefficients[1] + 1.96*reg2$cse[1],
    ymin = -Inf,
    ymax = Inf,
    fill = "orange",
    alpha = 0.02,
    inherit.aes = FALSE
  ) +
  geom_point(color = "orange", size = 2) + 
  geom_errorbar(width = 0.2, color = "orange") +
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  ylab("") +
  xlab("Point Estimate") +
  theme_bw() +
  theme(text = element_text(family = "Times"))

plot1

plot2 <- ggplot(ipa_results_df, 
                aes(y = country, x = coef, xmin = coef - 1.96*se, xmax = coef + 1.96*se)) +
  geom_rect(
    xmin = reg1$coefficients[1] - 1.96*reg1$cse[1],
    xmax = reg1$coefficients[1] + 1.96*reg1$cse[1],
    ymin = -Inf,
    ymax = Inf,
    fill = "darkgreen",
    alpha = 0.02,
    inherit.aes = FALSE
  ) +
  geom_point(color = "darkgreen", size = 2) + 
  geom_errorbar(width = 0.2, color = "darkgreen") +
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  ylab("") +
  xlab("Point Estimate") +
  theme_bw() +
  theme(text = element_text(family = "Times"))
plot2

### Save Results
ggsave(filename = paste0(exhibits, "jpal_leave_one_out_policy.jpeg"), plot = plot1,
       units = "cm", height = 10, width = 20)
ggsave(filename = paste0(exhibits, "ipa_leave_one_out_policy.jpeg"), plot = plot2,
       units = "cm", height = 10, width = 20)
