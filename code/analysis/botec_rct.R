



twfe_ols[["jpal"]] <- run_jpal_regressions(outcome = "frequency_rct", specification = "ols-event-study")
twfe_ols[["ipa"]] <- run_ipa_regressions(outcome = "frequency_rct", specification = "ols-event-study")




jpal_coefs <- twfe_ols[["jpal"]]$coefficients[25:42]
ipa_coefs <- twfe_ols[["ipa"]]$coefficients[24:45]



jpal_df <- data.frame(
  time_to_jpal = 0:17,
  jpal_coef = jpal_coefs
)

ipa_df <- data.frame(
  time_to_ipa = 0:21,
  ipa_coef = ipa_coefs
)

synthetic_rct_panel <- freqs_panel %>%
  dplyr::select(country, year, time_to_jpal, time_to_ipa, jpal_treated, ipa_treated, frequency_rct) %>%
  dplyr::left_join(jpal_df, by = "time_to_jpal") %>%
  dplyr::left_join(ipa_df, by = "time_to_ipa") %>%
  dplyr::mutate(
    `synthetic_rct` = dplyr::case_when(
      ipa_treated == 0 & jpal_treated == 0 ~ 0,
      jpal_treated == 1 & time_to_jpal >= 0 ~ jpal_coef,
      ipa_treated == 1 & time_to_ipa >= 0 ~ ipa_coef,
      TRUE ~ NA_real_))

    
plot <- synthetic_rct_panel %>% 
  group_by(year) %>%
  dplyr::summarize(
    `Total RCTs` = sum(frequency_rct),
    `Implied RCTs from Point Estimates` = sum(synthetic_rct)) %>%
  ungroup() %>%
  tidyr::pivot_longer(
    cols = c(`Total RCTs`, `Implied RCTs from Point Estimates`),
    names_to = "series",
    values_to = "value") %>%
  ggplot(., aes(x = year, y = value, color = series, fill = series)) +
  geom_line(size = 1.2) +
  geom_area(alpha = 0.25, position = "identity") +
  scale_fill_manual(values = c("blue", "black"), labels = c("Implied RCTs from Point Estimates", "Total RCTs")) +
  scale_color_manual(values = c("blue", "black"), labels = c("Implied RCTs from Point Estimates", "Total RCTs")) +
  theme_bw(base_size = 14) +
  labs(
    x = "Year",
    y = "Number of RCTs",
    color = "Series",
    fill = "Series") +
  theme(legend.position = "bottom",
        text = element_text(family = "Times"),
        legend.title = element_blank())

  
ggsave(plot = plot, filename = paste0(exhibits, "figures/share_rcts_jpal_ipa.jpeg"), 
       units = "cm", width = 18, height = 12)
    

number_of_rcts <- synthetic_rct_panel %>%
  dplyr::summarize(sum(synthetic_rct)) %>%
  round(.,0) %>%
  as.character()

writeLines(number_of_rcts, paste0(exhibits, "statistics/number_of_rcts.tex"))


share_rcts_jpal_ipa <- synthetic_rct_panel %>%
  dplyr::summarize(
    share = sum(synthetic_rct) / sum(frequency_rct)) %>%
  pull(share)*100

share_rcts_jpal_ipa <- as.character(round(share_rcts_jpal_ipa, 1))


writeLines(share_rcts_jpal_ipa, paste0(exhibits, "statistics/share_rcts_jpal_ipa.tex"))



