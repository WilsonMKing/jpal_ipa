

plot <- wos_metadata %>%
  group_by(year) %>%
  dplyr::summarize(n = sum(rct)) %>%
  ggplot(aes(x = year, y = n)) +
  geom_point(size = 2, shape = 5) + 
  geom_line() + 
  xlab("Year") + 
  ylab("Number of RCTs") +
  theme_bw() +
  theme(text = element_text(family = "Times", size = 14))
  
plot

ggsave(plot = plot, filename = paste0(exhibits, "number_of_rcts.jpeg"), units = "cm",
         width = 18, height = 12)
