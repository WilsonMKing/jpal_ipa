################################################################################
################################################################################
################################################################################

journals_list <- unique(wos_metadata$journal)

ipa_coefs <- rep(NA, length(journals_list))
jpal_coefs <- rep(NA, length(journals_list))

for(j in seq_along(journals_list)){
  
  freqs_panel <- create_freqs_panel(wos_metadata %>% dplyr::filter(journal != journals_list[j])) %>%
    dplyr::filter(year >= 1995 & year <= 2024)
  
  ipa <- run_ipa_regressions(outcome = "frequency_rct", specification = "ols-binary-did")
  jpal <- run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did")
  
  ipa_coefs[j] <- ipa$coefficients[1]
  jpal_coefs[j] <- jpal$coefficients[1]
}


results <- data.frame(
  journal = journals_list,
  ipa_coefs = ipa_coefs,
  jpal_coefs = jpal_coefs)

freqs_panel <- create_freqs_panel(wos_metadata) %>%
  dplyr::filter(year >= 1995 & year <= 2024)

reg1 <- run_ipa_regressions(outcome = "frequency_rct", specification = "ols-binary-did")
reg2 <- run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did")

ggplot(results, aes(x = ipa_coefs)) +
  geom_histogram(fill = "darkgreen", alpha = 0.6, color = "black") +
  geom_vline(xintercept = reg1$coefficients[1], linetype = 2) +
  annotate("text", x = reg1$coefficients[1], y = 170, 
           label = "Headline Estimated β", family = "Times", color = "black", size = 4, hjust = 1.1) +
  annotate("text",x = results[results$journal == "JOURNAL OF DEVELOPMENT ECONOMICS", ]$ipa_coefs[1], y = 8, 
           label = "JDE", family = "Times", color = "darkgreen", size = 4) +
  annotate("text",x = results[results$journal == "WORLD DEVELOPMENT", ]$ipa_coefs[1], y = 8, 
           label = "WD", family = "Times", color = "darkgreen", size = 4) +
  xlab("Estimated β") +
  ylab("Frequency") +
  theme_bw() +
  theme(text = element_text(family = "Times"))

ggsave(paste0(exhibits, "ipa_journal_leave_one_out.jpeg"), plot = last_plot(),
       units = "cm", width = 15, height = 10)

ggplot(results, aes(x = jpal_coefs)) +
  geom_histogram(fill = "orange", alpha = 0.6, color = "black") +
  geom_vline(xintercept = reg2$coefficients[1], linetype = 2) +
  annotate("text", x = reg2$coefficients[1], y = 130, 
           label = "Headline Estimated β", family = "Times", color = "black", size = 4, hjust = 1.1) +
  annotate("text",x = results[results$journal == "JOURNAL OF DEVELOPMENT ECONOMICS", ]$jpal_coefs[1]-0.01, y = 8, 
           label = "JDE", family = "Times", color = "orange", size = 4) +
  annotate("text",x = results[results$journal == "JOURNAL OF ECONOMIC BEHAVIOR & ORGANIZATION", ]$jpal_coefs[1], y = 8, 
           label = "JEBO", family = "Times", color = "orange", size = 4) +
  annotate("text",x = results[results$journal == "JOURNAL OF POLICY ANALYSIS AND MANAGEMENT", ]$jpal_coefs[1], y = 8, 
           label = "JPAM", family = "Times", color = "orange", size = 4) +
  xlab("Estimated β") +
  ylab("Frequency") +
  theme_bw() +
  theme(text = element_text(family = "Times"))

ggsave(paste0(exhibits, "jpal_journal_leave_one_out.jpeg"), plot = last_plot(),
       units = "cm", width = 15, height = 10)
