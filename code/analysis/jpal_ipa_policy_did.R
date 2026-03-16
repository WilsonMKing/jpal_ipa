################################################################################
####################### Create Table of J-PAL/IPA Results ######################
################################################################################

### Create Frequency Panel
freqs_panel <- create_freqs_panel(wos_metadata) %>%
  dplyr::filter(year >= 1995 & year <= 2024) %>%
  dplyr::filter(country %in% dev_world)

### Run Regressions

### Number of RCTs
reg1 <- run_ipa_regressions(outcome = "ifelse(frequency_policy_doc_rct > 0, 1, 0)", specification = "ols-binary-did")
reg2 <- run_jpal_regressions(outcome = "ifelse(frequency_policy_doc_rct > 0, 1, 0)", specification = "ols-binary-did")
reg3 <- run_jpal_regressions(outcome = "ifelse(frequency_policy_doc_rct > 0, 1, 0)", specification = "ols-binary-did-both")

### Create Table
table <- stargazer::stargazer(
  reg1, reg2, reg3,
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
    c("& \\multicolumn{3}{c}{At Least One Policy Document} \\\\",
      "\\cmidrule(lr){2-5}",
      paste0(
        "Control Mean & ", 
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0,]$frequency_policy_doc_rct > 0) / nrow(freqs_panel[freqs_panel$ever_ipa == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_jpal == 0,]$frequency_policy_doc_rct > 0) / nrow(freqs_panel[freqs_panel$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]$frequency_policy_doc_rct > 0) / nrow(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]),2), " \\\\"),
      "Controls & Yes & Yes & Yes \\\\"),
    insert.after = c(4,4,14,14))
table

### Save
starpolishr::star_tex_write(table, file = paste0(exhibits, "tables/jpal_ipa_policy_did.tex"))


