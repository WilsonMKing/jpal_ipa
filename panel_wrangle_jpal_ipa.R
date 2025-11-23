################################################################################
################### Create Table of Country-Level Frequencies ##################
################################################################################

### Merge World Bank and V-DEM Data
country_level <- 
  full_join(wb_data, vdem, by = c("country.code", "year")) %>%
  dplyr::select(-country.y) %>%
  dplyr::rename(country = country.x) %>%
  dplyr::filter(year >= 1975 & country.code %nin%
                  c("YMD", "TWN", "PSE", "DDR", "PSG", "SML", "HKG", "ZZB", "VDR"))

### Create Numeric Country ID Variable
country_level$country_id <- as.numeric(factor(country_level$country))

### Merge with DHS Data
country_level %<>% full_join(., dhs_surveys, by = "country") %>%
  dplyr::mutate(dhs = as.numeric(dhs), lsms = as.numeric(lsms))

### Merge with IPA/J-PAL/IGC Openings
country_level %<>% full_join(., igc_openings, by = "country") %>%
  full_join(., jpal_openings, by = "country") %>%
  full_join(., ipa_openings, by = "country")

### Create Additional J-PAL Coverage Variables
country_level %<>%
  dplyr::mutate(jpal_ext_year = case_when(
    country %in% jpal_europe ~ 2007,
    country %in% jpal_south_asia ~ 2007,
    country %in% jpal_lac ~ 2009,
    country %in% jpal_africa ~ 2011,
    country %in% jpal_southeast_asia ~ 2013,
    country %in% jpal_na ~ 2013,
    country %in% jpal_mena ~ 2020))

### Manual V-DEM Imputations
for(t in 1975:1990){
  country_level %<>%
    dplyr::mutate(
      regime = case_when(
        year == t & 
          country %in% c("Estonia", "Latvia", "Lithuania", "Belarus", "Ukraine", 
                         "Azerbaijan", "Georgia", "Armenia", "Kazakhstan", "Tajikistan", 
                         "Kyrgyz Republic", "Uzbekistan", "Turkmenistan", "Moldova") ~
          country_level$regime[country_level$country == "Russian Federation" & year == t],  # <- take first value
        TRUE ~ regime))}

for(t in 1975:1991){
  country_level %<>%
    dplyr::mutate(
      regime = case_when(
        year == t & 
          country %in% 
          c("Bosnia and Herzegovina", "Croatia", "Slovenia", "North Macedonia") ~
          country_level$regime[country_level$country == "Serbia" & year == t],
        TRUE ~ regime))}  

for(t in 1975:1997){
  country_level %<>%
    dplyr::mutate(
      regime = case_when(
        year == t & country == "Montenegro" ~ country_level$regime[country_level$country == "Serbia" & year == t],
        TRUE ~ regime))}

for(t in 1975:1998){
  country_level %<>%
    dplyr::mutate(
      regime = case_when(
        year == t & country == "Kosovo" ~ country_level$regime[country_level$country == "Serbia" & year == t],
        TRUE ~ regime))}

for(t in 1975:2010){
  country_level %<>%
    dplyr::mutate(
      regime = case_when(
        year == t & country == "South Sudan" ~ country_level$regime[country_level$country == "Sudan" & year == t],
        TRUE ~ regime))}

for(t in 1975:1992){
  country_level %<>%
    dplyr::mutate(
      regime = case_when(
        year == t & country == "Slovak Republic" ~ country_level$regime[country_level$country == "Czechia" & year == t],
        TRUE ~ regime))}

### Impute Missing Values
country_level %<>%
  dplyr::mutate(
    regime = case_when(
      is.na(regime) ~ "Missing",
      !is.na(regime) ~ regime))

### Merge with Independence Data
country_level <- left_join(country_level, independence, by = "country")
country_level %<>% dplyr::mutate(
  independent = case_when(
    is.na(independence_year) ~ 1,
    year >= independence_year ~ 1,
    TRUE ~ 0))

### Merge with Conflict Data
country_level <- left_join(country_level, conflict_data, by = c("country", "year")) %>%
  dplyr::mutate(conflict = ifelse(is.na(conflict), 0, conflict))

### Create Binary DHS Indicator
country_level %<>%
  dplyr::mutate(
    dhs_treated = case_when(
      is.na(dhs) ~ 0,
      !is.na(dhs) & dhs > year ~ 0,
      !is.na(dhs) & dhs <= year ~ 1),
    lsms_treated = case_when(
      is.na(lsms) ~ 0,
      !is.na(lsms) & lsms > year ~ 0,
      !is.na(lsms) & lsms <= year ~ 1),
    ipa_treated = case_when(
      is.na(ipa_year) ~ 0,
      !is.na(ipa_year) & ipa_year > year ~ 0,
      !is.na(ipa_year) & ipa_year <= year ~ 1),
    jpal_treated = case_when(
      is.na(jpal_year) ~ 0,
      !is.na(jpal_year) & jpal_year > year ~ 0,
      !is.na(jpal_year) & jpal_year <= year ~ 1),
    jpal_ext_treated = case_when(
      is.na(jpal_ext_year) ~ 0,
      !is.na(jpal_ext_year) & jpal_ext_year > year ~ 0,
      !is.na(jpal_ext_year) & jpal_ext_year <= year ~ 1),
    igc_treated = case_when(
      is.na(igc_year) ~ 0,
      !is.na(igc_year) & igc_year > year ~ 0,
      !is.na(igc_year) & igc_year <= year ~ 1))

### Create Ever DHS/IPA/IGC/J-PAL Indicator
country_level %<>%
  dplyr::mutate(
    ever_dhs = case_when(
      !is.na(dhs) ~ 1,
      is.na(dhs) ~ 0),
    ever_lsms = case_when(
      !is.na(lsms) ~ 1,
      is.na(lsms) ~ 0),
    ever_ipa = case_when(
      !is.na(ipa_year) ~ 1,
      is.na(ipa_year) ~ 0),
    ever_jpal = case_when(
      !is.na(jpal_year) ~ 1,
      is.na(jpal_year) ~ 0),
    ever_jpal_ext = case_when(
      !is.na(jpal_ext_year) ~ 1,
      is.na(jpal_ext_year) ~ 0),
    ever_igc = case_when(
      !is.na(igc_year) ~ 1,
      is.na(igc_year) ~ 0))

### Create "Time to X" Indicators
country_level %<>%
  dplyr::mutate(
    time_to_dhs = case_when(
      ever_dhs == 0 ~ 0,
      ever_dhs == 1 ~ year - dhs),
    time_to_lsms = case_when(
      ever_lsms == 0 ~ 0,
      ever_lsms == 1 ~ year - lsms),
    time_to_ipa = case_when(
      ever_ipa == 0 ~ 0,
      ever_ipa == 1 ~ year - ipa_year),
    time_to_igc = case_when(
      ever_igc == 0 ~ 0,
      ever_igc == 1 ~ year - igc_year),
    time_to_jpal = case_when(
      ever_jpal == 0 ~ 0,
      ever_jpal == 1 ~ year - jpal_year),
    time_to_jpal_ext = case_when(
      ever_jpal_ext == 0 ~ 0,
      ever_jpal_ext == 1 ~ year - jpal_ext_year))

### Create Binned Time to DHS Indicator
country_level %<>%
  dplyr::mutate(dhs_bins = case_when(
    #   time_to_dhs %in% -36:-27 ~ -14,
    #   time_to_dhs %in% -26:-25 ~ -13,
    #   time_to_dhs %in% -24:-23 ~ -12,
    #   time_to_dhs %in% -22:-21 ~ -11,
    #   time_to_dhs %in% -20:-19 ~ -10,
    #   time_to_dhs %in% -18:-17 ~ -9,
    #   time_to_dhs %in% -16:-15 ~ -8,
    #   time_to_dhs %in% -14:-13 ~ -7,
    #   time_to_dhs %in% -12:-11 ~ -6,
    time_to_dhs %in% -10:-9 ~ -5,
    time_to_dhs %in% -8:-7 ~ -4,
    time_to_dhs %in% -6:-5 ~ -3,
    time_to_dhs %in% -4:-3 ~ -2,
    time_to_dhs %in% -2:-1 ~ -1,
    time_to_dhs %in% 0:1 ~ 0,
    time_to_dhs %in% 2:3 ~ 1,
    time_to_dhs %in% 4:5 ~ 2,
    time_to_dhs %in% 6:7 ~ 3,
    time_to_dhs %in% 8:9 ~ 4,
    time_to_dhs %in% 10:11 ~ 5,
    time_to_dhs %in% 12:13 ~ 6,
    time_to_dhs %in% 14:15 ~ 7,
    time_to_dhs %in% 16:17 ~ 8,
    time_to_dhs %in% 18:19 ~ 9,
    time_to_dhs %in% 20:21 ~ 10,
    time_to_dhs %in% 22:23 ~ 11,
    time_to_dhs %in% 24:25 ~ 12,
    time_to_dhs %in% 26:27 ~ 13,
    time_to_dhs %in% 28:29 ~ 14,
    time_to_dhs %in% 30:31 ~ 15))
#   time_to_dhs %in% 32:33 ~ 16,
#   time_to_dhs %in% 34:37 ~ 17

country_level %<>%
  dplyr::mutate(ipa_bins = case_when(
    time_to_ipa %in% -37:-29 ~ -15,
    time_to_ipa %in% -28:-27 ~ -14,
    time_to_ipa %in% -26:-25 ~ -13,
    time_to_ipa %in% -24:-23 ~ -12,
    time_to_ipa %in% -22:-21 ~ -11,
    time_to_ipa %in% -20:-19 ~ -10,
    time_to_ipa %in% -18:-17 ~ -9,
    time_to_ipa %in% -16:-15 ~ -8,
    time_to_ipa %in% -14:-13 ~ -7,
    time_to_ipa %in% -12:-11 ~ -6,
    time_to_ipa %in% -10:-9 ~ -5,
    time_to_ipa %in% -8:-7 ~ -4,
    time_to_ipa %in% -6:-5 ~ -3,
    time_to_ipa %in% -4:-3 ~ -2,
    time_to_ipa %in% -2:-1 ~ -1,
    time_to_ipa %in% 0:1 ~ 0,
    time_to_ipa %in% 2:3 ~ 1,
    time_to_ipa %in% 4:5 ~ 2,
    time_to_ipa %in% 6:7 ~ 3,
    time_to_ipa %in% 8:9 ~ 4,
    time_to_ipa %in% 10:11 ~ 5,
    time_to_ipa %in% 12:13 ~ 6,
    time_to_ipa %in% 14:19 ~ 7))

country_level %<>%
  dplyr::mutate(igc_bins = case_when(
    time_to_igc %in% -41:-29 ~ -15,
    time_to_igc %in% -28:-27 ~ -14,
    time_to_igc %in% -26:-25 ~ -13,
    time_to_igc %in% -24:-23 ~ -12,
    time_to_igc %in% -22:-21 ~ -11,
    time_to_igc %in% -20:-19 ~ -10,
    time_to_igc %in% -18:-17 ~ -9,
    time_to_igc %in% -16:-15 ~ -8,
    time_to_igc %in% -14:-13 ~ -7,
    time_to_igc %in% -12:-11 ~ -6,
    time_to_igc %in% -10:-9 ~ -5,
    time_to_igc %in% -8:-7 ~ -4,
    time_to_igc %in% -6:-5 ~ -3,
    time_to_igc %in% -4:-3 ~ -2,
    time_to_igc %in% -2:-1 ~ -1,
    time_to_igc %in% 0:1 ~ 0,
    time_to_igc %in% 2:3 ~ 1,
    time_to_igc %in% 4:5 ~ 2,
    time_to_igc %in% 6:7 ~ 3,
    time_to_igc %in% 8:9 ~ 4,
    time_to_igc %in% 10:11 ~ 5,
    time_to_igc %in% 12:14 ~ 6))

country_level %<>%
  dplyr::mutate(jpal_bins = case_when(
    time_to_jpal %in% -31:-29 ~ -15,
    time_to_jpal %in% -28:-27 ~ -14,
    time_to_jpal %in% -26:-25 ~ -13,
    time_to_jpal %in% -24:-23 ~ -12,
    time_to_jpal %in% -22:-21 ~ -11,
    time_to_jpal %in% -20:-19 ~ -10,
    time_to_jpal %in% -18:-17 ~ -9,
    time_to_jpal %in% -16:-15 ~ -8,
    time_to_jpal %in% -14:-13 ~ -7,
    time_to_jpal %in% -12:-11 ~ -6,
    time_to_jpal %in% -10:-9 ~ -5,
    time_to_jpal %in% -8:-7 ~ -4,
    time_to_jpal %in% -6:-5 ~ -3,
    time_to_jpal %in% -4:-3 ~ -2,
    time_to_jpal %in% -2:-1 ~ -1,
    time_to_jpal %in% 0:1 ~ 0,
    time_to_jpal %in% 2:3 ~ 1,
    time_to_jpal %in% 4:5 ~ 2,
    time_to_jpal %in% 6:7 ~ 3,
    time_to_jpal %in% 8:9 ~ 4,
    time_to_jpal %in% 10:11 ~ 5,
    time_to_jpal %in% 12:15 ~ 6))

### Reconcile DHS Indicator
country_level %<>%
  dplyr::mutate(
    dhs = case_when(
      is.na(dhs) ~ 0,
      !is.na(dhs) ~ dhs),
    lsms = case_when(
      is.na(lsms) ~ 0,
      !is.na(lsms) ~ lsms),
    igc = case_when(
      is.na(igc_year) ~ 0,
      !is.na(igc_year) ~ igc_year),
    jpal = case_when(
      is.na(jpal_year) ~ 0,
      !is.na(jpal_year) ~ jpal_year),
    jpal_ext = case_when(
      is.na(jpal_ext_year) ~ 0,
      !is.na(jpal_ext_year) ~ jpal_ext_year),
    ipa = case_when(
      is.na(ipa_year) ~ 0,
      !is.na(ipa_year) ~ ipa_year))

### Create Number of DHS Rounds
dhs_years <- strsplit(as.character(country_level$dhs_years), ", ")
lsms_years <- strsplit(as.character(country_level$lsms_years), ", ")

### Function: Count # of Past DHS Rounds
count_past_years <- function(years, current_year) {
  sum(as.integer(years) <= current_year)}

### Count Rounds
country_level$no_surveys <- mapply(count_past_years, dhs_years, country_level$year)
country_level$no_surveys <- ifelse(is.na(country_level$no_surveys), 0, country_level$no_surveys)
country_level$no_lsms <- mapply(count_past_years, lsms_years, country_level$year)
country_level$no_lsms <- ifelse(is.na(country_level$no_lsms), 0, country_level$no_lsms)

### Create Dummy Indicators for Democracy
country_level %<>%
  dplyr::mutate(
    democracy = case_when(
      regime %in% c("Electoral Democracy", "Liberal Democracy") ~ 1,
      TRUE ~ 0))

### Create GDP per Capita Variable
country_level %<>%
  dplyr::mutate(
    gdp_pc = gdp / population)

### Create Asia and Africa Dummies
country_level %<>%
  dplyr::mutate(
    africa = ifelse(continent == "Africa", 1, 0),
    asia = ifelse(continent == "Asia", 1, 0),
    europe = ifelse(continent == "Europe", 1, 0),
    samerica = ifelse(continent == "South America", 1, 0))

### Create Max Surveys Variable
country_level %<>%
  group_by(country) %>%
  dplyr::mutate(max_surveys = max(no_surveys, na.rm = TRUE)) %>%
  ungroup()

### Create Variable for Number of Bordering Countries with the DHS
country_level$bordering_dhs <- mapply(
  count_bordering_dhs,
  country = 1:length(country_level$country),
  year = 1:length(country_level$year))

### Create Variable for Number of Bordering Countries with the DHS
country_level$bordering_ipa <- mapply(
  count_bordering_ipa,
  country = 1:length(country_level$country),
  year = 1:length(country_level$year))

### Create Variable for Number of Bordering Countries with the DHS
country_level$bordering_jpal <- mapply(
  count_bordering_jpal,
  country = 1:length(country_level$country),
  year = 1:length(country_level$year))

### Create Variable for At Least One Bordering Country with DHS
country_level$bordering_dhs_treated <- ifelse(country_level$bordering_dhs > 0, 1, 0)
country_level$bordering_ipa_treated <- ifelse(country_level$bordering_ipa > 0, 1, 0)
country_level$bordering_jpal_treated <- ifelse(country_level$bordering_jpal > 0, 1, 0)

create_freqs_panel <- function(dataset){
  
  ### Create Frequency Dataframe for 1995
  df0 <- dataset %>% dplyr::filter(year == 1995) %>%
    dplyr::filter(!is.na(topic)) %>% group_by(topic) %>% 
    dplyr::summarize(
      frequency = n(),
      frequency_rct = sum(rct))
  
  ### Add for Next 40 Years
  for(i in 1:29){
    df1 <- dataset %>% dplyr::filter(year == 1995+i) %>% dplyr::filter(!is.na(topic)) %>%
      group_by(topic) %>% 
      dplyr::summarize(
        frequency = n(),
        frequency_rct = sum(rct)) %>%
      ungroup()
    df0 <- full_join(df0, df1, by = "topic")}
  
  ### Clean Up Constructed Dataframe
  colnames(df0)[2:length(df0)] <- paste0(
    c("frequency_", "frequency_rct_"), rep(1995:2024, each=2))
  df0[is.na(df0)] <- 0
  df0 %<>% dplyr::rename(country = topic)
  
  ### Convert to Long Format and Merge Additional Panels
  df0 %<>% pivot_longer(cols = -country, names_to = c(".value", "year"), names_pattern = "(.*)_([0-9]+)") %>%
    dplyr::filter(!is.na(year)) %>%
    dplyr::mutate(year = as.numeric(year))
  
  freqs_panel <- full_join(df0, country_level, by = c("country", "year"))
  
  ### Merge with World Bank Data
  freqs_panel %<>%
    dplyr::mutate_at(
      vars("frequency", "frequency_rct"), 
      ~coalesce(.,0)) %>%
    dplyr::mutate(relative_freq = frequency / population)
  
  return(freqs_panel)
  
}

freqs_panel <- create_freqs_panel(wos_metadata)
