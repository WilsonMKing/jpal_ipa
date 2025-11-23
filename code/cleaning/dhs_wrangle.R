################################################################################
################# Wrangle Demographic & Health Surveys Meta-Data ###############
################################################################################

### Preliminaries

  ### Load DHS Survey Meta-Data
dhs_surveys <- readxl::read_excel(paste0(data, "dhs_surveys.xlsx"))

  ### Create Function to Select Earliest Survey Year
get_earliest <- function(variable){
  stringr::str_extract_all(variable, "[0-9\\.-]+") %>%
    lapply(., function(x) min(as.numeric(x), na.rm = TRUE)) %>%
    lapply(., function(x) if(identical(x, Inf)) NA_character_ else x) %>%
    unlist()
}

  ### Generate Variable for First DHS Survey Year
dhs_surveys %<>% 
  dplyr::mutate(
    dhs = get_earliest(dhs_surveys$years),
    dhs_hiv = as.numeric(get_earliest(dhs_surveys$hiv_years)),
    dhs_malaria = as.numeric(get_earliest(dhs_surveys$malaria_years)),
    lsms = get_earliest(dhs_surveys$lsms_years)) %>%
  dplyr::rename(dhs_years = years)

