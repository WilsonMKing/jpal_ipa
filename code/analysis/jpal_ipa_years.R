################################################################################
################## 
################################################################################


all_years <- 2002:2021  # adjust this to your desired range

freqs_panel %>%
  dplyr::select(country, jpal_year, ipa_year) %>%
  distinct() %>%
  pivot_longer(
    cols = c(jpal_year, ipa_year),
    names_to = "source",
    values_to = "year"
  ) %>%
  ggplot(aes(x = year, fill = source)) +
  geom_bar(alpha = 0.6, color = "black") +  # stacked by default
  labs(x = "Year", y = "Number of Office Openings", fill = "Source") +
  theme_classic() +
  theme(
    legend.position = c(0.8,0.8), 
    legend.title = element_blank(),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(1, "cm"),
    text = element_text(family = "Times", size = 12)) +
  scale_fill_manual(
    values = c("jpal_year" = "orange", "ipa_year" = "darkgreen"),
    labels = c("jpal_year" = "J-PAL", "ipa_year" = "IPA"))


### Save
ggsave(paste0(exhibits, "figures/jpal_ipa_years.jpeg"), plot = last_plot(), units = "cm",
       width = 18, height = 12)
