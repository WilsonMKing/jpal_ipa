################################################################################
################## Create Plot of J-PAL/IPA Event Study Results ################
################################################################################

### Create Frequency Panel
freqs_panel <- create_freqs_panel(wos_data) %>%
  dplyr::filter(year >= 1995)

### Estimate OLS Fixed Effects Model

### Create Storage
twfe_ols <- list()
twfe_sa <- list()

### Run Regressions

  ### J-PAL
twfe_ols[["jpal_education"]] <- run_jpal_regressions(outcome = "frequency_education_experiments", specification = "ols-event-study")
twfe_sa[["jpal_education"]] <- run_jpal_regressions(outcome = "frequency_education_experiments", specification = "sa-event-study")
twfe_ols[["jpal_agriculture"]] <- run_jpal_regressions(outcome = "frequency_agriculture_experiments", specification = "ols-event-study")
twfe_sa[["jpal_agriculture"]] <- run_jpal_regressions(outcome = "frequency_agriculture_experiments", specification = "sa-event-study")
twfe_ols[["jpal_health"]] <- run_jpal_regressions(outcome = "frequency_health_experiments", specification = "ols-event-study")
twfe_sa[["jpal_health"]] <- run_jpal_regressions(outcome = "frequency_health_experiments", specification = "sa-event-study")

### Save Results
event_study_results <- 
  data.frame(
    outcome = 
      c(rep("Education", 52), 
        rep("Agriculture", 52),
        rep("Health", 52)),
    coef = 
      c(as.numeric(twfe_ols[["jpal_education"]]$coefficients[16:40]), 0, 
        as.numeric(twfe_sa[["jpal_education"]]$coeftable[,1][16:40]), 0,
        as.numeric(twfe_ols[["jpal_agriculture"]]$coeftable[,1][16:40]), 0,
        as.numeric(twfe_sa[["jpal_agriculture"]]$coeftable[,1][16:40]), 0,
        as.numeric(twfe_ols[["jpal_health"]]$coeftable[,1][16:40]), 0,
        as.numeric(twfe_sa[["jpal_health"]]$coeftable[,1][16:40]), 0),
    se = 
      c(as.numeric(twfe_ols[["jpal_education"]]$se[16:40]), 0, 
        as.numeric(se(twfe_sa[["jpal_education"]])[16:40]), 0,
        as.numeric(twfe_ols[["jpal_agriculture"]]$se[16:40]), 0, 
        as.numeric(se(twfe_sa[["jpal_agriculture"]])[16:40]), 0,
        as.numeric(twfe_ols[["jpal_health"]]$coeftable[,1][16:40]), 0,
        as.numeric(twfe_sa[["jpal_health"]]$coeftable[,1][16:40]), 0),
    estimator = 
      c(rep("OLS", 26), 
        rep("Sun & Abraham", 26),
        rep("OLS", 26), 
        rep("Sun & Abraham", 26),
        rep("OLS", 26), 
        rep("Sun & Abraham", 26)),
    bins = c(
      -10:-2, 0:15, -1, 
      -10:-2, 0:15, -1,
      -10:-2, 0:15, -1, 
      -10:-2, 0:15, -1,
      -10:-2, 0:15, -1, 
      -10:-2, 0:15, -1))

### Plot Results: Panel A-B
ggplot(event_study_results %>%
         dplyr::filter(outcome == "Health" | outcome == "Education" | outcome == "Agriculture"), 
       aes(x = bins, y = coef, ymin = coef - 1.96*se, ymax = coef + 1.96*se, color = estimator, shape = estimator)) +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_line(position = position_dodge(width = 0.8)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.8)) +
  geom_vline(xintercept = -1, linetype = 2) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("OLS" = "black", "Sun & Abraham" = "#0066CC")) +
  xlab("Years Relative to Treatment") +
  ylab("Number of RCTs") +
  theme_classic() +
  theme(legend.position = "none",
        legend.title = element_blank()) +
  facet_wrap(~outcome, nrow = 1, scales = "free_y")

ggsave(filename = paste0(exhibits, "figures/jpal_topics_event_study.jpeg"), plot = last_plot(),
       units = "cm", width = 20, height = 6)

################################################################################

### Estimate OLS Fixed Effects Model

### Create Storage
twfe_ols <- list()
twfe_sa <- list()

### Run Regressions

### J-PAL
twfe_ols[["ipa_education"]] <- run_ipa_regressions(outcome = "frequency_education_experiments", specification = "ols-event-study")
twfe_sa[["ipa_education"]] <- run_ipa_regressions(outcome = "frequency_education_experiments", specification = "sa-event-study")
twfe_ols[["ipa_agriculture"]] <- run_ipa_regressions(outcome = "frequency_agriculture_experiments", specification = "ols-event-study")
twfe_sa[["ipa_agriculture"]] <- run_ipa_regressions(outcome = "frequency_agriculture_experiments", specification = "sa-event-study")
twfe_ols[["ipa_health"]] <- run_ipa_regressions(outcome = "frequency_health_experiments", specification = "ols-event-study")
twfe_sa[["ipa_health"]] <- run_ipa_regressions(outcome = "frequency_health_experiments", specification = "sa-event-study")

### Save Results
event_study_results <- 
  data.frame(
    outcome = 
      c(rep("Education", 52), 
        rep("Agriculture", 52),
        rep("Health", 52)),
    coef = 
      c(as.numeric(twfe_ols[["ipa_education"]]$coefficients[15:39]), 0, 
        as.numeric(twfe_sa[["ipa_education"]]$coeftable[,1][15:39]), 0,
        as.numeric(twfe_ols[["ipa_agriculture"]]$coeftable[,1][15:39]), 0,
        as.numeric(twfe_sa[["ipa_agriculture"]]$coeftable[,1][15:39]), 0,
        as.numeric(twfe_ols[["ipa_health"]]$coeftable[,1][15:39]), 0,
        as.numeric(twfe_sa[["ipa_health"]]$coeftable[,1][15:39]), 0),
    se = 
      c(as.numeric(twfe_ols[["ipa_education"]]$se[15:39]), 0, 
        as.numeric(se(twfe_sa[["ipa_education"]])[15:39]), 0,
        as.numeric(twfe_ols[["ipa_agriculture"]]$se[15:39]), 0, 
        as.numeric(se(twfe_sa[["ipa_agriculture"]])[15:39]), 0,
        as.numeric(twfe_ols[["ipa_health"]]$coeftable[,1][15:39]), 0,
        as.numeric(twfe_sa[["ipa_health"]]$coeftable[,1][15:39]), 0),
    estimator = 
      c(rep("OLS", 26), 
        rep("Sun & Abraham", 26),
        rep("OLS", 26), 
        rep("Sun & Abraham", 26),
        rep("OLS", 26), 
        rep("Sun & Abraham", 26)),
    bins = c(
      -10:-2, 0:15, -1, 
      -10:-2, 0:15, -1,
      -10:-2, 0:15, -1, 
      -10:-2, 0:15, -1,
      -10:-2, 0:15, -1, 
      -10:-2, 0:15, -1))

### Plot Results: Panel A-B
ggplot(event_study_results %>%
         dplyr::filter(outcome == "Health" | outcome == "Education" | outcome == "Agriculture"), 
       aes(x = bins, y = coef, ymin = coef - 1.96*se, ymax = coef + 1.96*se, color = estimator, shape = estimator)) +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_line(position = position_dodge(width = 0.8)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.8)) +
  geom_vline(xintercept = -1, linetype = 2) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("OLS" = "black", "Sun & Abraham" = "#0066CC")) +
  xlab("Years Relative to Treatment") +
  ylab("Number of RCTs") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  facet_wrap(~outcome, nrow = 1, scales = "free_y")

ggsave(filename = paste0(exhibits, "figures/ipa_topics_event_study.jpeg"), plot = last_plot(),
       units = "cm", width = 20, height = 6)
