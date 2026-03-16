

################################################################################

### Create Analysis Dataset
freqs_panel <- create_freqs_panel(wos_metadata) %>%
  dplyr::filter(year >= 1995 & year <= 2024) %>%
  dplyr::filter(country %in% dev_world)

### Estimates BJS Event Study: IPA
ipa_reg <- didimputation::did_imputation(
  data = freqs_panel,
  yname = "frequency_policy_doc_rct",
  gname = "ipa_year",
  tname = "year",
  idname = "country_id",
  horizon = TRUE,
  pretrends = TRUE)

ipa_df <- ipa_reg %<>% dplyr::mutate(term = as.numeric(term))

### Add Reference Year
ipa_df[nrow(ipa_df) + 1,] <- list("frequency_rct", -1, 0, 0, 0, 0)

### Trim Event Study
ipa_df %<>% dplyr::filter(term >= -10 & term <= 12)

### Plot
ggplot(ipa_df, aes(x = as.numeric(term), y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(color = "darkgreen", size = 0.4) +
  geom_ribbon(fill = "darkgreen", alpha = 0.4) +
  geom_vline(xintercept = -1, linetype = 2) +
  geom_line(color = "darkgreen") +
  xlab("Years Relative to Treatment") +
  ylab("Number of Policy Documents Referencing RCTs") +
  theme_bw() +
  theme(text = element_text("Times"))

ggsave(paste0(exhibits, "ipa_bjs_event_study_policy.jpeg"), plot = last_plot(),
       units = "cm", width = 15, height = 10)

################################################################################

### Estimate BJS Event Study: J-PAL
jpal_df <- didimputation::did_imputation(
  data = freqs_panel,
  yname = "frequency_policy_doc_rct",
  gname = "jpal_year",
  tname = "year",
  idname = "country_id",
  horizon = TRUE,
  pretrends = TRUE) %>%
  dplyr::mutate(term = as.numeric(term))

### Add Reference Year
jpal_df[nrow(jpal_df) + 1,] <- list("frequency_rct", -1, 0, 0, 0, 0)

### Trim Event Study
jpal_df %<>% dplyr::filter(term >= -10 & term <= 12)

ggplot(jpal_df, aes(x = as.numeric(term), y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(color = "orange", size = 0.4) +
  geom_ribbon(fill = "orange", alpha = 0.4) +
  geom_vline(xintercept = -1, linetype = 2) +
  geom_line(color = "orange") +
  xlab("Years Relative to Treatment") +
  ylab("Number of Policy Documents Referencing RCTs") +
  theme_bw() +
  theme(text = element_text("Times"))


ggsave(paste0(exhibits, "jpal_bjs_event_study_policy.jpeg"), plot = last_plot(),
       units = "cm", width = 15, height = 10)

