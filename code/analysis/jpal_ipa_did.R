################################################################################
####################### Create Table of J-PAL/IPA Results ######################
################################################################################

### Create Frequency Panel
freqs_panel <- create_freqs_panel(wos_metadata) %>%
  dplyr::filter(year >= 1995 & year <= 2024)

### Run Regressions

  ### At Least One RCT
reg4 <- run_ipa_regressions(outcome = "ifelse(frequency_rct > 0, 1, 0)", specification = "ols-binary-did")
reg5 <- run_jpal_regressions(outcome = "ifelse(frequency_rct > 0, 1, 0)", specification = "ols-binary-did")
reg6 <- run_jpal_regressions(outcome = "ifelse(frequency_rct > 0, 1, 0)", specification = "ols-binary-did-both")

  ### Number of RCTs
reg1 <- run_ipa_regressions(outcome = "frequency_rct", specification = "ols-binary-did")
reg2 <- run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did")
reg3 <- run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did-both")

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
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0,]$frequency_rct) / nrow(freqs_panel[freqs_panel$ever_ipa == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_jpal == 0,]$frequency_rct) / nrow(freqs_panel[freqs_panel$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]$frequency_rct) / nrow(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0,]$frequency_rct > 0) / nrow(freqs_panel[freqs_panel$ever_ipa == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_jpal == 0,]$frequency_rct > 0) / nrow(freqs_panel[freqs_panel$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]$frequency_rct > 0) / nrow(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]),2), " \\\\"),
      "Controls & Yes & Yes & Yes & Yes & Yes & Yes \\\\"),
    insert.after = c(4,4,14,14))
table

### Save
starpolishr::star_tex_write(table, file = paste0(exhibits, "tables/jpal_ipa_did.tex"))
