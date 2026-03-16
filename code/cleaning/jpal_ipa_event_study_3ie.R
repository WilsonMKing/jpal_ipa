################################################################################
#################### Re-Estimate Results with 3ie Meta-Data ####################
################################################################################

### Binary DID

### Run Regressions

### At Least One RCT
reg4 <- run_ipa_regressions(outcome = "ifelse(number_3ie > 0, 1, 0)", specification = "ols-binary-did")
reg5 <- run_jpal_regressions(outcome = "ifelse(number_3ie > 0, 1, 0)", specification = "ols-binary-did")
reg6 <- run_jpal_regressions(outcome = "ifelse(number_3ie > 0, 1, 0)", specification = "ols-binary-did-both")

### Number of RCTs
reg1 <- run_ipa_regressions(outcome = "number_3ie", specification = "ols-binary-did")
reg2 <- run_jpal_regressions(outcome = "number_3ie", specification = "ols-binary-did")
reg3 <- run_jpal_regressions(outcome = "number_3ie", specification = "ols-binary-did-both")

### Create Table
table <- stargazer::stargazer(
  reg1, reg2, reg3,
  reg4, reg5, reg6,
  align = TRUE,
  header = FALSE,
  digits = 2,
  float = FALSE,
  omit = c("Constant", "population", "gdp"),
  covariate.labels = c("IPA Office", "J-PAL Office"),
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  omit.stat = c("rsq", "adj.rsq", "ser"),
  omit.table.layout = "n") %>%
  starpolishr::star_insert_row(
    c("& \\multicolumn{3}{c}{Number of RCTs} & \\multicolumn{3}{c}{At Least One RCT} \\\\",
      "\\cmidrule(lr){2-7}",
      paste0(
        "Control Mean & ", 
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0,]$number_3ie) / nrow(freqs_panel[freqs_panel$ever_ipa == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_jpal == 0,]$number_3ie) / nrow(freqs_panel[freqs_panel$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]$number_3ie) / nrow(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0,]$number_3ie > 0) / nrow(freqs_panel[freqs_panel$ever_ipa == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_jpal == 0,]$number_3ie > 0) / nrow(freqs_panel[freqs_panel$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]$number_3ie > 0) / nrow(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]),2), " \\\\"),
      "Controls & Yes & Yes & Yes & Yes & Yes & Yes \\\\"),
    insert.after = c(4,4,14,14))
table

### Save
starpolishr::star_tex_write(table, file = paste0(exhibits, "tables/jpal_ipa_did_3ie.tex"))

################################################################################

### Event Studies

### Create Frequency Panel
freqs_panel <- create_freqs_panel(wos_metadata) %>%
  dplyr::filter(year >= 1995 & year <= 2024)

### Estimate OLS Fixed Effects Model

### Create Storage
twfe_ols <- list()
twfe_sa <- list()

### Run Regressions
twfe_ols[["jpal"]] <- run_jpal_regressions(outcome = "number_3ie", specification = "ols-event-study")
twfe_sa[["jpal"]] <- run_jpal_regressions(outcome = "number_3ie", specification = "sa-event-study")
twfe_ols[["ipa"]] <- run_ipa_regressions(outcome = "number_3ie", specification = "ols-event-study")
twfe_sa[["ipa"]] <- run_ipa_regressions(outcome = "number_3ie", specification = "sa-event-study")

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


ggsave(filename = paste0(exhibits, "figures/ipa_event_study_3ie.jpeg"), plot = last_plot(),
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


ggsave(filename = paste0(exhibits, "figures/jpal_event_study_3ie.jpeg"), plot = last_plot(),
       units = "cm", width = 15, height = 10)
