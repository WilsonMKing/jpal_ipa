################################################################################
######################## Master Analysis: Bibliometrics ########################
################################################################################

### Preliminaries
options(scipen=10000)

### File Paths
root <- paste0("/Users/", Sys.getenv("USER"), "/Documents/GitHub/jpal_ipa/")
code <- paste0(root, "code/")
functions <- paste0(code, "functions/")
cleaning <- paste0(code, "cleaning/")
analysis <- paste0(code, "analysis/")
data <- paste0("/Users/", Sys.getenv("USER"), "/Dropbox/data/")
exhibits <- paste0(root, "exhibits/")

### Load Packages
packages <- c("haven", "magrittr", "tidyr", "readxl", "purrr", "here", "ggplot2",
              "dplyr", "DescTools", "nnet", "stringi", "readr",  "stringr", "rvest",
              "stabs", "data.table", "colorspace", "sf", "doParallel", "openxlsx",
              "XML", "fixest", "extraFont", "rnaturalearth", "parallel",
              "JCRImpactFactor", "stargazer", "starpolishr")

### Function: Install Missing Packages
install_if_missing <- function(packages) {
  for (p in packages) {
    if (!requireNamespace(p, quietly = TRUE)) {message("Installing ", p)
      install.packages(p, dependencies = TRUE)}}}

### Install/Load Packages
install_if_missing(packages)
lapply(packages, require, character.only = TRUE)

### User Defined Functions
source(paste0(functions, "regex.R")) # String-search algorithm
source(paste0(functions, "run_ipa_regressions.R")) # Run regressions

### Data Cleaning
source(paste0(cleaning, "wb_wrangle.R")) # World Development Indicators
source(paste0(cleaning, "jpal_ipa_igc_wrangle.R")) # IGC meta-data
source(paste0(cleaning, "dhs_wrangle.R")) # DHS meta-data
source(paste0(cleaning, "independence_wrangle.R")) # National independence years
source(paste0(cleaning, "ucdp_wrangle.R")) # Uppsala Conflict Data Program data
source(paste0(cleaning, "vdem_wrangle.R")) # V-DEM data
# source(paste0(cleaning, "web_of_science_wrangle.R")) # Web of Science meta-data

### Assign Countries
set.seed(125161)
wos_data$topic <- mapply(assign_country, title = wos_data$title, abstract = wos_data$abstract)

### Save (Assigned) Web of Science Meta-Data
# save(wos_data, file = paste0(data, "wos_assigned_jpal_ipa.RData"))
# load(file = paste0(data, "wos_assigned_jpal_ipa.RData"))

### Filter to Economics Journals
# source(paste0(cleaning, "journal_names.R"))
# wos_data %<>% dplyr::filter(journal %in% demography_journals)

### Merge with Impact Factors
# source(paste0(cleaning, "if_wrangle.R"))

### Create Citation Quantiles
# source(paste0(cleaning, "citation_quantiles.R"))

### Merge Institutions Data
# source(paste0(cleaning, "institutions_wrangle.R"))

### Assign Keywords
# source(paste0(cleaning, "keyword_wrangle.R"))

### Create Vectors of Bordering Countries
source(paste0(cleaning, "borders.R"))

### Create Panel Dataset of Country-Level Frequencies
source(paste0(cleaning, "freqs_panel_wrangle.R"))
