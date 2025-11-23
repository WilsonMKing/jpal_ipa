


### Create Panel of Frequencies
freqs_by_country <- wos_metadata %>% 
  dplyr::filter(year >= 1995 & year <= 2024) %>%
  group_by(topic) %>%
  dplyr::summarize(n = sum(rct)) %>%
  dplyr::rename(country = topic)

pop_by_country <- create_freqs_panel(wos_metadata) %>%
  dplyr::select(c(country, population)) %>%
  group_by(country) %>%
  dplyr::summarize(population = mean(population))

df <- left_join(freqs_by_country, pop_by_country, by = "country") %>%
  dplyr::mutate(rcts_per_capita = (n / population)*100000000)

### Download World Shapefile
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = c("sf"))

### Clean Up Country Names
world %<>% dplyr::select(featurecla:geometry) %>%
  dplyr::rename(country = name_long) %>%
  dplyr::mutate(
    country = case_when(
      country == "Democratic Republic of the Congo" ~ "Congo, Dem. Rep.",
      country == "United States of America" ~ "United States",
      country == "Russia" ~ "Russian Federation",
      country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
      country == "Egypt" ~ "Egypt, Arab Rep.",
      country == "Iran" ~ "Iran, Islamic Rep.",
      country == "United Republic of Tanzania" ~ "Tanzania",
      country == "Turkey" ~ "Turkiye",
      country == "Republic of Korea" ~ "Korea, Rep.",
      country == "Czech Republic" ~ "Czechia",
      country == "Bahamas" ~ "Bahamas, The",
      country == "Slovakia" ~ "Slovak Republic",
      country == "Venezuela" ~ "Venezuela, RB",
      country == "Kyrgyzstan" ~ "Kyrgyz Republic",
      country == "São Tomé and Principe" ~ "Sao Tome and Principe",
      country == "Dem. Rep. Korea" ~ "Korea, Dem. People's Rep.",
      country == "East Timor" ~ "Timor-Leste",
      country == "Republic of Serbia" ~ "Serbia",
      country == "Federated States of Micronesia" ~ "Micronesia, Fed. Sts.",
      country == "Cape Verde" ~ "Cabo Verde",
      country == "Laos" ~ "Lao PDR",
      country == "Yemen" ~ "Yemen, Rep.",
      country == "Republic of the Congo" ~ "Congo, Rep.",
      country == "Syria" ~ "Syrian Arab Republic",
      country == "Eswatini" ~ "Kingdom of eSwatini",
      country == "Macedonia" ~ "North Macedonia",
      country == "The Gambia" ~ "Gambia, The",
      country == "Guinea Bissau" ~ "Guinea-Bissau",
      TRUE ~ country))

### Merge with Shapefile
map_df <- full_join(world, df, by = "country")

### Plot Map
map_df %>% 
  filter(country != "Antarctica") %>%
  mutate(rcts_per_capita = case_when(
    rcts_per_capita == 0 ~ NA_real_,  # use NA_real_ for numeric
    TRUE ~ rcts_per_capita
  )) %>%
  ggplot(aes(fill = log(rcts_per_capita))) + 
  geom_sf(color = "gray", size = 0.01) +
  scale_fill_gradient(
    low = "gray95",
    high = "blue",
    na.value = "gray95"
  ) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.68, 0.2),
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    text = element_text(family = "Times", size = 12),
    legend.key = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  )

### Save
ggsave(paste0(exhibits, "figures/rcts_map.jpeg"), plot = last_plot(), units = "cm",
       width = 18, height = 12)
