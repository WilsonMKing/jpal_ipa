################################################################################
################## Create Plot of J-PAL/IPA Event Study Results ################
################################################################################

### Create Frequency Panel
freqs_panel <- create_freqs_panel(wos_data) %>%
  dplyr::filter(year >= 2013)

### Estimate OLS Fixed Effects Model

### Create Storage
twfe_ols <- list()
twfe_sa <- list()

### Run Regressions
twfe_ols[["jpal"]] <- run_jpal_regressions(outcome = "aea_rct_registry_frequency", specification = "ols-event-study")
twfe_sa[["jpal"]] <- run_jpal_regressions(outcome = "aea_rct_registry_frequency", specification = "sa-event-study")
twfe_ols[["ipa"]] <- run_ipa_regressions(outcome = "aea_rct_registry_frequency", specification = "ols-event-study")
twfe_sa[["ipa"]] <- run_ipa_regressions(outcome = "aea_rct_registry_frequency", specification = "sa-event-study")

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

### Plot Results: Panel A-B
ggplot(event_study_results %>%
         dplyr::filter(outcome == "Abdul Latif Jameel Poverty Action Lab" |
                         outcome == "Innovations for Poverty Action"), 
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
  facet_wrap(~outcome, nrow = 2, scales = "free_y")

ggsave(filename = paste0(exhibits, "figures/jpal_ipa_aea_rct_registry_event_study.jpeg"), plot = last_plot(),
       units = "cm", width = 15, height = 15)
