################################################################################
##################### Wrangle World Development Indicators #####################
################################################################################

### Read in Country-Level Data

  ### Gross Domestic Product
wb_gdp <- readr::read_csv(paste0(data, "world bank/wb_gdp_wide.csv")) %>%
  pivot_longer(
    cols = matches("^\\d{4} \\[YR\\d{4}\\]$"),
    names_to = "year",
    values_to = "value") %>%
  dplyr::mutate(
    year = substr(year, 1, 4),
    value = na_if(value, ".."),
    value = as.numeric(value)) %>% 
  dplyr::select(-c(`Series Name`, `Series Code`)) %>%
  dplyr::rename(gdp = value) %>%
  dplyr::filter(!is.na("Country Name")) %>%
  dplyr::mutate(`Country Name` = case_when(`Country Name` == "Viet Nam" ~ "Vietnam", 
                                           `Country Name` == "Somalia, Fed. Rep." ~ "Somalia",
                                           TRUE ~ `Country Name`))

  ### Population
wb_population <- readr::read_csv(paste0(data, "world bank/wb_population_wide.csv")) %>%
  dplyr::mutate(
    dplyr::across(matches("^\\d{4} \\[YR\\d{4}\\]$"), as.character)
  ) %>%
  pivot_longer(
    cols = matches("^\\d{4} \\[YR\\d{4}\\]$"),
    names_to = "year",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    year = substr(year, 1, 4),
    value = gsub(",", "", value),
    value = na_if(value, ".."),
    value = as.numeric(value)
  ) %>%
  dplyr::select(-c(`Series Name`, `Series Code`)) %>%
  dplyr::rename(population = value) %>%
  dplyr::filter(!is.na(`Country Name`)) %>%
  dplyr::mutate(`Country Name` = case_when(`Country Name` == "Viet Nam" ~ "Vietnam", 
                                           `Country Name` == "Somalia, Fed. Rep." ~ "Somalia",
                                           TRUE ~ `Country Name`))

  ### Statistical Capacity
wb_spi <- readr::read_csv(paste0(data, "world bank/wb_spi.csv")) %>% 
  dplyr::select(c(country, SPI.INDEX)) %>% 
  dplyr::rename(`Country Name` = country, spi = SPI.INDEX) %>%
  dplyr::mutate(`Country Name` = case_when(`Country Name` == "Viet Nam" ~ "Vietnam", 
                                           `Country Name` == "Somalia, Fed. Rep." ~ "Somalia",
                                           TRUE ~ `Country Name`))

### Merge World Bank Data
wb_data <- full_join(wb_gdp, wb_population, by = c("Country Name", "Country Code", "year"))
wb_data <- full_join(wb_data, wb_spi, by = "Country Name")

### Rename Columns
colnames(wb_data)[1:2] <- c("country", "country.code")

### Remove Extraneous Rows
wb_data %<>% dplyr::filter(!is.na(country))

### Generate Dummy Indicators for Country / Region

  ### Establish Vectors of Countries
asia <- c("Afghanistan" ,"Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan",
          "Brunei Darussalam", "Burundi", "Cambodia", "China", "Georgia", "India",
          "Indonesia", "Iran, Islamic Rep.", "Iraq", "Israel", "Japan", "Kazakhstan",
          "Kuwait", "Kyrgyz Republic", "Lao PDR", "Lebanon", "Malaysia", "Maldives",
          "Mongolia", "Myanmar", "Nepal", "Korea, Dem. People's Rep.", "Oman", "Pakistan",
          "Philippines", "Qatar", "Saudi Arabia", "Singapore", "Korea, Rep.", "Sri Lanka",
          "Syrian Arab Republic", "Tajikistan", "Thailand", "Turkiye", "Vietnam",
          "United Arab Emirates", "Jordan", "Turkmenistan", "Uzbekistan", "Yemen, Rep.")
europe <- c("Iceland", "United Kingdom", "Ireland", "Portugal", "Spain", "Andorra",
            "France", "Italy", "San Marino", "Switzerland", "Liechtenstein", "Luxembourg",
            "Belgium", "Netherlands", "Germany", "Denmark", "Norway", "Sweden", "Finland",
            "Russian Federation", "Estonia", "Latvia", "Lithuania", "Belarus", "Ukraine",
            "Moldova", "Bulgaria", "Romania", "North Macedonia", "Greece", "Montenegro",
            "Albania", "Bosnia and Herzegovina", "Croatia", "Slovenia", "Slovak Republic",
            "Czechia", "Austria", "Hungary", "Serbia", "Kosovo", "Malta", "Cyprus", "Poland",
            "Monaco")
africa <- c("Egypt, Arab Rep.", "Libya", "Algeria", "Tunisia", "Morocco", "Mauritania",
            "Mali", "Chad", "Niger", "Sudan", "South Sudan", "Eritrea", "Djibouti",
            "Somalia", "Kenya", "Tanzania", "Mozambique", "Eswatini", "South Africa",
            "Lesotho", "Zambia", "Zimbabwe", "Botswana", "Namibia", "Angola", "Congo, Rep.",
            "Congo, Dem. Rep.", "Central African Republic", "Gabon", "Cameroon", "Equatorial Guinea",
            "Guinea", "Guinea-Bissau", "Gambia, The", "Senegal", "Sierra Leone", "Liberia", "Nigeria",
            "Ghana", "Togo", "Benin", "Cote d'Ivoire", "Burkina Faso", "Uganda", "Rwanda", "Burundi",
            "Ethiopia", "Somalia", "Comoros", "Seychelles", "Sao Tome and Principe", "Mauritius",
            "Cabo Verde", "Madagascar", "Malawi")
southam <- c("Brazil", "Argentina", "Peru", "Colombia", "Venezuela, RB", "Guyana", "Suriname",
             "Ecuador", "Bolivia", "Paraguay", "Uruguay", "Chile", "Argentina")
northam <- c("Canada", "United States", "Mexico", "Guatemala", "Belize", "El Salvador",
             "Nicaragua", "Honduras", "Costa Rica", "Panama", "Cuba", "Dominican Republic",
             "Haiti", "Jamaica", "St. Lucia", "St. Vincent and the Grenadines",
             "St. Kitts and Nevis", "Antigua and Barbuda", "Barbados", "Bahamas, The",
             "Trinidad and Tobago", "Dominica", "Grenada")
oceania <- c("Australia", "New Zealand", "Papua New Guinea", "Timor-Leste", "Fiji",
             "Kiribati", "Nauru", "Tonga", "Samoa", "Tuvalu", "Vanuatu", "Solomon Islands",
             "Marshall Islands", "Micronesia, Fed. Sts.", "Palau")
lac <- c(southam, "Mexico", "Guatemala", "Belize", "El Salvador",
         "Nicaragua", "Honduras", "Costa Rica", "Panama", "Cuba", "Dominican Republic",
         "Haiti", "Jamaica", "St. Lucia", "St. Vincent and the Grenadines",
         "St. Kitts and Nevis", "Antigua and Barbuda", "Barbados", "Bahamas, The",
         "Trinidad and Tobago", "Dominica", "Grenada")

  ### Create Dummy Indicators
wb_data %<>%
  dplyr::mutate(
    continent = case_when(
      country %in% asia ~ "Asia",
      country %in% europe ~ "Europe",
      country %in% oceania ~ "Oceania",
      country %in% southam ~ "South America",
      country %in% northam ~ "North America",
      country %in% africa ~ "Africa",
      TRUE ~ "Nothing"),
    region = case_when(
      country %in% asia | country %in% oceania ~ "Asia-Pacific",
      country %in% lac ~ "Latin America & Caribbean",
      country %in% africa ~ "Africa",
      (country %in% northam | country %in% europe) & !(country %in% lac) ~ "Europe, U.S., and Canada"))
wb_data %<>% dplyr::filter(continent != "Nothing")

### Create Dummy Indicator for English Speaking Countries

  ### Create Vector of English Speaking Countries
english <- c("Antigua and Barbuda", "Australia", "Bahamas, The", "Barbados",
             "Belize", "Canada", "Dominica", "Grenada", "Guyana", "Ireland",
             "Jamaica", "Malta", "New Zealand", "St. Kitts and Nevis", "St. Lucia",
             "St. Vincent and the Grenadines", "Trinidad and Tobago", "United Kingdom",
             "United States")

  ### Assign Dummy Indicator
wb_data %<>%
  dplyr::mutate(
    english = case_when(
      country %in% english ~ 1,
      TRUE ~ 0))

### Convert WB Indicators to Numeric
wb_data %<>%
  dplyr::mutate(
    year = as.numeric(year),
    population = as.numeric(population),
    gdp = as.numeric(gdp))

### Remove Non-Countries
wb_data %<>%
  dplyr::filter(
    country %nin%
      c("American Samoa", "Aruba", "Bermuda", "British Virgin Islands", "Cayman Islands",
        "Channel Islands", "Curacao", "Faroe Islands", "French Polynesia", "Gibraltar",
        "Greenland", "Guam", "Hong Kong SAR, China", "Isle of Man", "Macao SAR, China",
        "New Caledonia", "Northern Mariana Islands", "Puerto Rico", "Sint Maarten (Dutch part)",
        "St. Martin (French part)", "Turks and Caicos Islands", "Virgin Islands (U.S.)",
        "West Bank and Gaza"))

### Load Maddison Income Dataset
maddison <- haven::read_dta(paste0(data, "maddison_north_korea.dta")) %>%
  dplyr::select(-country) %>%
  dplyr::rename(country.code = countrycode) %>%
  dplyr::filter(country.code == "PRK")

### Left Join with World Bank Data
wb_data <- left_join(wb_data, maddison, by = c("year", "country.code"))
wb_data %<>%
  dplyr::mutate(
    gdp = case_when(
      country == "Korea, Dem. People's Rep." ~ gdppc*1.3*population,
      TRUE ~ gdp))

### Replace NAs with Closest Value
wb_data %<>% 
  group_by(country) %>%
  tidyr::fill(gdp, .direction = "downup") %>%
  tidyr::fill(population, .direction = "downup")

### Remove GDP per Capita
wb_data$gdppc <- NULL
wb_data$pop <- NULL


################################################################################

### Create List of World Bank Developing Countries
wb_classes <- readxl::read_xlsx(paste0(data, "world bank/OGHIST.xlsx"), sheet = "Country Analytical History")
wb_classes <- wb_classes[-c(1:10),] %>% 
  dplyr::mutate(`World Bank Analytical Classifications` = case_when(
    `World Bank Analytical Classifications` == "Türkiye" ~ "Turkiye",
    `World Bank Analytical Classifications` == "Côte d'Ivoire" ~ "Cote d'Ivoire",
    `World Bank Analytical Classifications` == "São Tomé and Príncipe" ~ "Sao Tome and Principe",
    `World Bank Analytical Classifications` == "Viet Nam" ~ "Vietnam",
    `World Bank Analytical Classifications` == "Czech Republic" ~ "Czechia",
    `World Bank Analytical Classifications` == "Korea, Dem. Rep." ~ "Korea, Dem. People's Rep.",
    TRUE ~ `World Bank Analytical Classifications`)) %>%
  dplyr::mutate_all(~ ifelse(. == "..", NA, .))
dev_world <- subset(wb_classes, !apply(wb_classes[, -c(1:2)] == "H", 1, all))$`World Bank Analytical Classifications`

