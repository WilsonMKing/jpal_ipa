################################################################################
############# Create Map of Density of Policy Documents by Country #############
################################################################################

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(readr)
library(countrycode)
library(scales)

### Load Overton Data
overton_N <- readr::read_csv(paste0(data, "overton/overton_sample_size.csv")) %>%
  dplyr::filter(!(country %in% c("IGO", "Hong Kong", "Taiwan", "EU")))

### Convert Country Codes
df <- overton_N %>%
  mutate(iso3 = countrycode(country, origin = "country.name", destination = "iso3c"))

### Load World Map
world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  dplyr::mutate(iso_a3 = case_when(
    admin == "France" ~ "FRA", 
    admin == "Norway" ~ "NOR",
    admin == "Somaliland" ~ "SOM",
    admin == "Kosovo" ~ "KOS",
    TRUE ~ iso_a3))

### Merge Data
map_data <- world %>%
  left_join(df, by = c("iso_a3" = "iso3")) %>%
  dplyr::filter(continent != "Antarctica")

### Plot Results
ggplot(map_data) +
  geom_sf(aes(fill = frequency), color = "white", size = 0.1) +
  scale_fill_gradient(
    low = "grey85",
    high = "blue",
    trans = "log10",
    labels = scales::comma,
    na.value = "grey90"
  ) +
  labs(
    fill = "Frequency") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    text = element_text(family = "serif"))

ggsave(paste0(exhibits, "figures/overton_map.jpeg"), units = "cm",
       height = 12, width = 23)
