################################################################################
################# Wrangle International Growth Centre Meta-Data ################
################################################################################

### Load IGC Openings Data
igc_openings <- readxl::read_excel(paste0(data, "igc.xlsx"))

### Load J-PAL Openings Data
jpal_openings <- readxl::read_excel(paste0(data, "jpal.xlsx"))

### Load IPA Openings Data
ipa_openings <- readxl::read_excel(paste0(data, "ipa.xlsx"))

### Create Vectors of J-PAL Countries
jpal_south_asia <- c("India", "Nepal", "Bhutan", "Bangladesh", "Pakistan", "Afghanistan")
jpal_southeast_asia <- c("Myanmar", "Thailand", "Laos", "Cambodia", "Vietnam", "Timor-Leste",
                         "Indonesia", "Brunei Darussalam", "Singapore")
jpal_mena <- c("Morocco", "Algeria", "Tunisia", "Libya", "Egypt, Arab Rep.",
               "Lebanon", "Jordan", "Syrian Arab Republic", "Iraq", "Iran, Islamic Rep.",
               "Saudi Arabia", "Yemen", "Oman", "United Arab Emirates", "Qatar", "Bahrain", "Kuwait")
jpal_lac <- c(southam, "Panama", "Costa Rica", "Nicaragua", "Honduras", "El Salvador",
              "Guatemala", "Belize", "Mexico", "Cuba", "Jamaica", "Haiti", "Dominican Republic",
              "Bahamas, The", "Trinidad and Tobago", "St. Kitts and Nevis", "Antigua and Barbuda",
              "Dominica", "St. Lucia", "Barbados", "Grenada")
jpal_na <- c("United States", "Canada")
jpal_europe <- c(europe, "Kazakhstan", "Turkmenistan", "Uzbekistan", "Tajikistan", "Kyrgyz Republic", "Georgia",
                 "Azerbaijan", "Armenia", "Turkiye")
jpal_africa <- setdiff(africa, c("Morocco", "Algeria", "Tunisia", "Libya", "Egypt, Arab Rep."))
