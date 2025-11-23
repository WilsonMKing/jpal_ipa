################################################################################
###################### Web of Science Meta-Data Wrangling ######################
################################################################################

### Web of Science Meta-Data Wrangling
wos_paths <- list.files(path = paste0(data, "web of science/econ-journals"), pattern = "wos_", 
                        recursive = TRUE, full.names = TRUE)

### Read in Meta-Data
wos_data_raw <- data.frame()
for(i in seq_along(wos_paths)){
  df <- readr::read_delim(wos_paths[[i]])
  wos_data_raw <- rbind(wos_data_raw, df)
}

### Preliminary Cleaning

  ### Select and Rename Variables of Interest
wos_data <- wos_data_raw %>%
  dplyr::select(c(AF, TI, SO, LA, DT, ID, AB, C3, FU, TC, PU, PD, PY, VL, C1, FX)) %>%
  dplyr::rename(
    authors = AF,
    title = TI,
    journal = SO,
    language = LA,
    document = DT,
    keywords = ID,
    abstract = AB,
    institutions = C3,
    funder = FU,
    cited = TC,
    publisher = PU,
    address = C1,
    month = PD,
    year = PY,
    volume = VL,
    footnote = FX
  )

  ### Remove Corrections / Editorial Material / Book Reviews
wos_data %<>%
  dplyr::filter(
    document %nin% 
      c("Editorial Material", "Book Review", "Correction",
        "Biographical-Item", "Retraction", "Bibliography",
        "Book", "Item Withdrawal", "News Item", "Software Review",
        "Correction; Early Access", "Article; Retracted Publications",
        "Editorial Material; Early Access", "Review", "Reprint",
        "Article; Book Chapter", "Article; Early Access", "Article; Retracted Publication",
        "Editorial Material; Book Chapter", "Article; Proceedings Paper",
        "Meeting Abstract", "Item Withdrawal; Withdrawn Publication", "Book Review; Early Access",
        "Biographical-Item; Early Access", "Correction, Addition", "Item About an Individual",
        "Database Review", "Note", "Review; Book Chapter"))

  ### Remove Extraneous Years
wos_data %<>% dplyr::filter(year > 1981 & year < 2023)

  ### Add Empty Column for Country Assignment
wos_data %<>% dplyr::mutate(topic = NA)

  ### Remove Capitalized Titles
wos_data %<>%
  dplyr::mutate(
    title = case_when(
      !is_uppercase(title) ~ title,
      is_uppercase(title) ~ str_to_title(title))) %>%
  dplyr::mutate(
    title = gsub("\\b(Us)\\b", "US", title, perl = TRUE),
    title = gsub("\\b(Uk)\\b", "UK", title, perl = TRUE),
    title = gsub("\\b(Uae)\\b", "UAE", title, perl = TRUE),
    title = gsub("\\b(Nhs)\\b", "NHS", title, perl = TRUE),
    title = gsub("\\b(Tanf)\\b", "TANF", title, perl = TRUE)) %>%
  dplyr::mutate(
    title = tools::toTitleCase(title))


