################################################################################
###################### Create Descriptive Statistics Table #####################
################################################################################

desc_table <- freqs_panel %>%
  ungroup() %>%
  dplyr::mutate(
    gdp = gdp / 1000000000,
    population = population / 1000000,
    relative_freq = relative_freq * 1e6,
    any_paper = ifelse(frequency > 0, 1, 0),
    any_rct = ifelse(frequency_rct > 0, 1, 0),
    any_policy_doc_rct = ifelse(frequency_policy_doc_rct > 0, 1, 0)) %>%   # scale first
  summarise(
    across(
      .cols = c(
        ever_jpal, ever_ipa, ever_igc, frequency, any_paper, relative_freq, frequency_rct, any_rct, number_3ie, frequency_policy_doc_rct, 
        any_policy_doc_rct, gdp, gdp_pc, population, democracy, conflict, english, samerica, africa, asia),
      .fns = list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        p10  = ~quantile(.x, 0.10, na.rm = TRUE),
        p90  = ~quantile(.x, 0.90, na.rm = TRUE)),
      .names = "{.col}_{.fn}")) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "statistic"),
    names_pattern = "^(.*)_(mean|sd|median|p10|p90)$",
    values_to = "value") %>%
  pivot_wider(
    names_from = statistic,
    values_from = value) %>%
  mutate(across(c(mean, sd, median, p10, p90), ~as.numeric(.x))) %>%
  mutate(across(c(mean, sd, median, p10, p90), ~round(.x, 2))) %>%
  dplyr::mutate(
    variable = case_when(
      variable == "ever_jpal" ~ "Ever J-PAL Office",
      variable == "ever_ipa" ~ "Ever IPA Office",
      variable == "ever_igc" ~ "Ever IGC Office",
      variable == "frequency" ~ "Number of Publications",
      variable == "any_paper" ~ "At Least One Publication",
      variable == "relative_freq" ~ "Publications per Million",
      variable == "frequency_rct" ~ "Number of Published RCTs",
      variable == "any_rct" ~ "At Least One Published RCT",
      variable == "number_3ie" ~ "Number of 3ie Indexed Papers",
      variable == "frequency_policy_doc_rct" ~ "Number of Policy Documents Referencing RCTs",
      variable == "any_policy_doc_rct" ~ "At Least One Policy Document Referencing RCTs",
      variable == "gdp" ~ "GDP (Current USD, Billions)",
      variable == "gdp_pc" ~ "GDP per Capita",
      variable == "population" ~ "Population (Millions)",
      variable == "democracy" ~ "Democratic",
      variable == "conflict" ~ "Conflict Site",
      variable == "english" ~ "Anglophone",
      variable == "samerica" ~ "South American",
      variable == "africa" ~ "African",
      variable == "asia" ~ "Asian"
    )
  )

colnames(desc_table) <- c("Variable", "Mean", "SD",  "Median", "10th", "90th")



# 2. Output with stargazer
stargazer::stargazer(
  desc_table,
  summary = FALSE,
  label = "tab:descriptive_statistics",
  rownames = FALSE,
  float = FALSE,
 # notes = "This table reports summary statistics across a number of the variables examined in this study. The unit of observation is the country-year. Data are from 1995 to 2024.",
  title = "Descriptive Statistics") %>%
  starpolishr::star_tex_write(file = paste0(exhibits, "tables/descriptive_statistics.tex"))
