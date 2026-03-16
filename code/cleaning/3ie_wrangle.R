



  
files <- list.files(paste0("/Users/", Sys.getenv("USER"), "/Documents/GitHub/site-selection/data/3ie"), full.names = TRUE)

### Function: Process 3ie Spreadsheets
process_3ie <- function(filename){
  
  ### Read 3ie Meta-Data
  df <- readxl::read_xlsx(filename)
  
  ### Set Country Name
  df$country <- sub(".*3ie_(.*)\\.xlsx$", "\\1", filename)
  
  ### Rename Year Variable
  df %<>% dplyr::rename(year = `Year of publication`)
  
  ### Return Dataframe
  return(df)
  
}


all_files_3ie <- list()
all_files_3ie <- lapply(files, process_3ie)
  
data_3ie <- do.call(rbind, all_files_3ie) %>%
  dplyr::mutate(
    country = case_when(
      country == "India_1" | country == "India_2" ~ "India",
      country == "Syria" ~ "Syrian Arab Republic",
      country == "Viet Nam" ~ "Vietnam",
      country == "Turkey" ~ "Turkiye",
      country == "Republic of Congo" ~ "Congo, Rep.",
      country == "Gambia" ~ "Gambia, The",
      country == "Macedonia" ~ "North Macedonia",
      country ==  "Kyrgyzstan" ~ "Kyrgyz Republic",
      country == "DRC" ~ "Congo, Dem. Rep.",
      TRUE ~ country)) %>%
  group_by(country, year) %>%
  dplyr::summarize(number_3ie = n())


