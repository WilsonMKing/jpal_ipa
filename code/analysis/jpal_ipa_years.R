################################################################################
################## 
################################################################################


all_years <- 2002:2021  # adjust this to your desired range

freqs_panel %>%
  dplyr::select(jpal, ipa) %>%
  pivot_longer(cols = c(jpal, ipa), names_to = "source", values_to = "year") %>%
  filter(year != 0) %>%
  ggplot(aes(x = factor(year, levels = all_years), fill = source)) +
  geom_bar(alpha = 0.6, color = "black") +  # stacked by default
  labs(x = "Year", y = "Number of Office Openings", fill = "Source") +
  theme_classic() +
  theme(
    legend.position = c(0.8,0.8), 
    legend.title = element_blank(),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(1, "cm"),
    text = element_text(family = "Times", size = 12)) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(labels = function(x) x / 100, breaks = c(0, 100, 200))+
  scale_fill_manual(
    values = c("jpal" = "orange", "ipa" = "darkgreen"),
    labels = c("jpal" = "J-PAL", "ipa" = "IPA"))


### Save
ggsave(paste0(exhibits, "figures/jpal_ipa_years.jpeg"), plot = last_plot(), units = "cm",
       width = 18, height = 12)
