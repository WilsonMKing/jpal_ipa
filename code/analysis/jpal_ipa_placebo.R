

### Create Frequency Panel
freqs_panel <- create_freqs_panel(wos_metadata) %>%
  dplyr::filter(year >= 1995 & year <= 2024) %>%
  dplyr::filter(country %in% dev_world)

### Number of Diff-in-Diffs
did_ipa <- run_ipa_regressions(outcome = "frequency_did", specification = "ols-binary-did")
did_jpal <- run_jpal_regressions(outcome = "frequency_did", specification = "ols-binary-did")

### Number of RDDs
rdd_ipa <- run_ipa_regressions(outcome = "frequency_rdd", specification = "ols-binary-did")
rdd_jpal <- run_jpal_regressions(outcome = "frequency_rdd", specification = "ols-binary-did")

### Number of Lab Experiments
lab_ipa <- run_ipa_regressions(outcome = "frequency_lab", specification = "ols-binary-did")
lab_jpal <- run_jpal_regressions(outcome = "frequency_lab", specification = "ols-binary-did")

### Create Table
table <- stargazer::stargazer(
  did_ipa, did_jpal,
  rdd_ipa, rdd_jpal,
  lab_ipa, lab_jpal,
  align = TRUE,
  header = FALSE,
  digits = 2,
  float = FALSE,
  omit = c("Constant", "population", "gdp"),
  covariate.labels = c("IPA Office", "J-PAL Office"),
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  omit.stat = c("rsq", "adj.rsq", "ser"),
  omit.table.layout = "n")  %>%
  starpolishr::star_insert_row(
    c("& \\multicolumn{2}{c}{Number of DIDs} & \\multicolumn{2}{c}{Number of RDDs} & \\multicolumn{2}{c}{Number of Lab Experiments} \\\\",
      "\\cmidrule(lr){2-7}",
      paste0(
        "Control Mean & ", 
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0,]$frequency_did, na.rm = TRUE) / nrow(freqs_panel[freqs_panel$ever_ipa == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_jpal == 0,]$frequency_did, na.rm = TRUE) / nrow(freqs_panel[freqs_panel$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0,]$frequency_rdd, na.rm = TRUE) / nrow(freqs_panel[freqs_panel$ever_ipa == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_jpal == 0,]$frequency_rdd, na.rm = TRUE) / nrow(freqs_panel[freqs_panel$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0,]$frequency_lab, na.rm = TRUE) / nrow(freqs_panel[freqs_panel$ever_ipa == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_jpal == 0,]$frequency_lab, na.rm = TRUE) / nrow(freqs_panel[freqs_panel$ever_jpal == 0,]),2), " \\\\"),
      "Controls & Yes & Yes & Yes & Yes & Yes & Yes \\\\"),
    insert.after = c(4,4,14,14))
