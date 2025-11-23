################################################################################
################## Create Plot of J-PAL/IPA Event Study Results ################
################################################################################

### Create Frequency Panel
freqs_panel <- create_freqs_panel(wos_metadata) %>%
  dplyr::filter(year >= 1995 & year <= 2024)

### Estimate OLS Fixed Effects Model

### Create Storage
twfe_ols <- list()
twfe_sa <- list()

### Run Regressions
twfe_ols[["jpal"]] <- run_jpal_regressions(outcome = "frequency_rct", specification = "ols-event-study")
twfe_sa[["jpal"]] <- run_jpal_regressions(outcome = "frequency_rct", specification = "sa-event-study")
twfe_ols[["ipa"]] <- run_ipa_regressions(outcome = "frequency_rct", specification = "ols-event-study")
twfe_sa[["ipa"]] <- run_ipa_regressions(outcome = "frequency_rct", specification = "sa-event-study")

### Save Results
event_study_results <- 
  data.frame(
    outcome = 
      c(rep("Abdul Latif Jameel Poverty Action Lab", 52), 
        rep("Innovations for Poverty Action", 52)),
    coef = 
      c(as.numeric(twfe_ols[["jpal"]]$coefficients[16:40]), 0, 
        as.numeric(twfe_sa[["jpal"]]$coeftable[,1][16:40]), 0,
        as.numeric(twfe_ols[["ipa"]]$coeftable[,1][15:39]), 0,
        as.numeric(twfe_sa[["ipa"]]$coeftable[,1][15:39]), 0),
    se = 
      c(as.numeric(twfe_ols[["jpal"]]$se[16:40]), 0, 
        as.numeric(se(twfe_sa[["jpal"]])[16:40]), 0,
        as.numeric(twfe_ols[["ipa"]]$se[15:39]), 0, 
        as.numeric(se(twfe_sa[["ipa"]])[15:39]), 0),
    estimator = 
      c(rep("OLS", 26), 
        rep("Sun & Abraham", 26),
        rep("OLS", 26), 
        rep("Sun & Abraham", 26)),
    bins = c(
      -10:-2, 0:15, -1, 
      -10:-2, 0:15, -1,
      -10:-2, 0:15, -1, 
      -10:-2, 0:15, -1))

pd <- position_dodge(width = 0.8)

event_study_results %>%
  dplyr::filter(outcome == "Innovations for Poverty Action") %>%
  mutate(bins_f = factor(bins, levels = sort(unique(bins)))) %>%
  ggplot(aes(x = bins_f, y = coef, group = estimator, linetype = estimator)) +
  geom_errorbar(
    aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se),
    position = pd,
    width = 0.2,
    color = "darkgreen"
  ) +
  geom_line(position = pd, color = "darkgreen", linewidth = 1) +
  geom_point(position = pd, color = "darkgreen", size = 1) +
  geom_vline(
    xintercept = which(sort(unique(event_study_results$bins)) == -1),
    linetype = 2
  ) +
  geom_hline(yintercept = 0) +
  xlab("Years Relative to Treatment") +
  ylab("Number of RCTs") +
  theme_classic() +
  theme(
    legend.position = c(0.2, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    text = element_text(family = "Times")
  )


ggsave(filename = paste0(exhibits, "figures/ipa_event_study.jpeg"), plot = last_plot(),
       units = "cm", width = 15, height = 10)

pd <- position_dodge(width = 0.8)

event_study_results %>%
  dplyr::filter(outcome == "Abdul Latif Jameel Poverty Action Lab") %>%
  mutate(bins_f = factor(bins, levels = sort(unique(bins)))) %>%
  ggplot(aes(x = bins_f, y = coef, group = estimator, linetype = estimator)) +
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), position = pd, width = 0.2, color = "orange") +
  geom_line(position = pd, color = "orange", linewidth = 1) +
  geom_point(position = pd, color = "orange", size = 1) +
  # use a fixed numeric position: the index of "-1" in the factor
  geom_vline(
    xintercept = which(sort(unique(event_study_results$bins)) == -1),
    linetype = 2
  ) +
  geom_hline(yintercept = 0) +
  xlab("Years Relative to Treatment") +
  ylab("Number of RCTs") +
  theme_classic() +
  theme(legend.position = c(0.2,0.8),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        text = element_text(family = "Times"))


ggsave(filename = paste0(exhibits, "figures/jpal_event_study.jpeg"), plot = last_plot(),
       units = "cm", width = 15, height = 10)





