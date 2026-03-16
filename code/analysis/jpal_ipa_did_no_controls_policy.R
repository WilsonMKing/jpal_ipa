################################################################################
####################### Create Table of J-PAL/IPA Results ######################
################################################################################

### Create Frequency Panel
freqs_panel <- create_freqs_panel(wos_metadata) %>%
  dplyr::filter(year >= 1995 & year <= 2024) %>%
  dplyr::filter(country %in% dev_world)

### Set Formulas
form1 <- as.formula("frequency_policy_doc_rct ~ ipa_treated + jpal_treated | as.factor(country) + as.factor(year) + as.factor(independent) | 0 | country")
form2 <- as.formula("frequency_policy_doc_rct ~ ipa_treated + jpal_treated + log(population) | as.factor(country) + as.factor(year) + as.factor(independent) | 0 | country")
form3 <- as.formula("frequency_policy_doc_rct ~ ipa_treated + jpal_treated + log(population) + log(gdp) | as.factor(country) + as.factor(year) + as.factor(independent) | 0 | country")
form4 <- as.formula("frequency_policy_doc_rct ~ ipa_treated + jpal_treated + log(population) + log(gdp) | as.factor(country) + as.factor(year) + as.factor(independent) + as.factor(conflict) | 0 | country")
form5 <- as.formula("frequency_policy_doc_rct ~ ipa_treated + jpal_treated + log(population) + log(gdp) | as.factor(country) + as.factor(year) + as.factor(independent) + as.factor(conflict) + as.factor(regime) | 0 | country")
form6 <- as.formula("frequency_policy_doc_rct ~ ipa_treated + jpal_treated + log(population) + log(gdp) | as.factor(country) + as.factor(year) + as.factor(independent) + as.factor(conflict) + as.factor(regime) + as.factor(igc_treated) | 0 | country")

### Run Regressions
reg1 <- lfe::felm(form1, data = freqs_panel)
reg2 <- lfe::felm(form2, data = freqs_panel)
reg3 <- lfe::felm(form3, data = freqs_panel)
reg4 <- lfe::felm(form4, data = freqs_panel)
reg5 <- lfe::felm(form5, data = freqs_panel)
reg6 <- lfe::felm(form6, data = freqs_panel)

### Create Table
table <- stargazer::stargazer(
  reg1, reg2, reg3,
  reg4, reg5, reg6,
  align = TRUE,
  header = FALSE,
  digits = 2,
  float = FALSE,
  omit = c("Constant"),
  covariate.labels = c("IPA Office", "J-PAL Office", "Logged Population", "Logged GDP"),
  dep.var.labels.include = FALSE,
  dep.var.caption = "",
  omit.stat = c("rsq", "adj.rsq", "ser"),
  omit.table.layout = "n") %>%
  starpolishr::star_insert_row(
    c("& \\multicolumn{6}{c}{Number of Policy Documents Referencing RCTs} \\\\",
      "\\cmidrule(lr){2-7}",
      "Conflict FEs & No & No & No & Yes & Yes & Yes \\\\",
      "Regime FEs & No & No & No & No & Yes & Yes \\\\",
      "IGC FEs & No & No & No & No & No & Yes \\\\",
      "\\hline",
      paste0(
        "Control Mean & ", 
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]$frequency_policy_doc_rct) / nrow(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]$frequency_policy_doc_rct) / nrow(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]$frequency_policy_doc_rct) / nrow(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]$frequency_policy_doc_rct) / nrow(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]$frequency_policy_doc_rct) / nrow(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]$frequency_policy_doc_rct) / nrow(freqs_panel[freqs_panel$ever_ipa == 0 & freqs_panel$ever_jpal == 0,]),2), " \\\\")),
    insert.after = c(4,4,19,19,19,19,20))
table

### Save
starpolishr::star_tex_write(table, file = paste0(exhibits, "tables/jpal_ipa_did_no_controls_policy.tex"))
