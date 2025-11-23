################################################################################
################## Create Table of J-PAL/IPA Results by Topic ##################
################################################################################

### Create Frequency Panel
freqs_panel <- create_freqs_panel(wos_data) %>%
  dplyr::filter(year >= 1995)

### IPA
ipa1 <- run_ipa_regressions(outcome = "frequency_agriculture_experiments", specification = "ols-binary-did")
ipa2 <- run_ipa_regressions(outcome = "frequency_health_experiments", specification = "ols-binary-did")
ipa3 <- run_ipa_regressions(outcome = "frequency_education_experiments", specification = "ols-binary-did")

### J-PAL
jpal1 <- run_jpal_regressions(outcome = "frequency_agriculture_experiments", specification = "ols-binary-did")
jpal2 <- run_jpal_regressions(outcome = "frequency_health_experiments", specification = "ols-binary-did")
jpal3 <- run_jpal_regressions(outcome = "frequency_education_experiments", specification = "ols-binary-did")

### Create Table
table <- stargazer::stargazer(
  ipa1, jpal1, 
  ipa2, jpal2,
  ipa3, jpal3, 
  align = TRUE,
  header = FALSE,
  digits = 2,
  float = FALSE,
  omit = c("Constant", "population", "gdp"),
  covariate.labels = c("IPA Office", "J-PAL Office"),
  dep.var.labels.include = FALSE,
  dep.var.caption = "",
  omit.stat = c("rsq", "adj.rsq", "ser"),
  omit.table.layout = "n") %>%
  starpolishr::star_insert_row(
    c("& \\multicolumn{2}{c}{Agriculture} & \\multicolumn{2}{c}{Health} & \\multicolumn{2}{c}{Education} \\\\",
      "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
      paste0(
        "Control Mean & ", 
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0,]$frequency_agriculture_experiments) / nrow(freqs_panel[freqs_panel$ever_ipa == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_jpal == 0,]$frequency_agriculture_experiments) / nrow(freqs_panel[freqs_panel$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0,]$frequency_health_experiments) / nrow(freqs_panel[freqs_panel$ever_ipa == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_jpal == 0,]$frequency_health_experiments) / nrow(freqs_panel[freqs_panel$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0,]$frequency_education_experiments) / nrow(freqs_panel[freqs_panel$ever_ipa == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_jpal == 0,]$frequency_education_experiments) / nrow(freqs_panel[freqs_panel$ever_jpal == 0,]),2)," \\\\"),
      "Controls & Yes & Yes & Yes & Yes & Yes & Yes \\\\"),
    insert.after = c(4,4,14,14))

### Save
starpolishr::star_tex_write(table, file = paste0(exhibits, "tables/jpal_ipa_did_by_topic.tex"))
