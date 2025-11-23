################################################################################
################# Create Table of J-PAL/IPA Spillovers Results #################
################################################################################

### Create Frequency Panel
freqs_panel <- create_freqs_panel(wos_metadata) %>%
  dplyr::filter(year >= 1995 & year <= 2024)

### Run Regressions

  ### Intensive Margin
reg1 <- run_ipa_regressions(outcome = "frequency_rct", specification = "ols-binary-did-spillovers")
reg2 <- run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did-spillovers")

  ### Extensive Margin
reg3 <- run_ipa_regressions(outcome = "ifelse(frequency_rct > 0, 1, 0)", specification = "ols-binary-did-spillovers")
reg4 <- run_jpal_regressions(outcome = "ifelse(frequency_rct > 0, 1, 0)", specification = "ols-binary-did-spillovers")

### Create Table
table <- stargazer::stargazer(
  reg1, reg2, reg3, reg4,
  align = TRUE,
  header = FALSE,
  digits = 2,
  float = FALSE,
  omit = c("Constant", "population", "gdp"),
  covariate.labels = c("IPA Office", "Bordering IPA Office", "J-PAL Office", "Bordering J-PAL Office"),
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  omit.stat = c("rsq", "adj.rsq", "ser"),
  omit.table.layout = "n") %>%
  starpolishr::star_insert_row(
    c("& \\multicolumn{2}{c}{Number of RCTs} & \\multicolumn{2}{c}{At Least One RCT} \\\\",
      "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
      paste0(
        "Control Mean & ", 
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0,]$frequency_rct) / nrow(freqs_panel[freqs_panel$ever_ipa == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_jpal == 0,]$frequency_rct) / nrow(freqs_panel[freqs_panel$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0,]$frequency_rct > 0) / nrow(freqs_panel[freqs_panel$ever_ipa == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_jpal == 0,]$frequency_rct > 0) / nrow(freqs_panel[freqs_panel$ever_jpal == 0,]),2), " \\\\"),
      "Controls & Yes & Yes & Yes & Yes \\\\"),
    insert.after = c(4,4,20,20))

### Save
starpolishr::star_tex_write(table, file = paste0(exhibits, "tables/jpal_ipa_did_spillovers.tex"))
