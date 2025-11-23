################################################################################
########################### Wrangle Independence Data ########################## 
################################################################################

### Create Dataframe of National Independence Years
independence <- data.frame(
  country = c(
    # 1976
    "Seychelles",
    # 1977
    "Djibouti",
    # 1978
    "Dominica", "Tuvalu", "Solomon Islands",
    # 1979
    "St. Vincent and the Grenadines", "Kiribati", "St. Lucia",
    # 1980
    "Vanuatu",
    # 1981
    "Antigua and Barbuda", "Belize",
    # 1983
    "St. Kitts and Nevis",
    # 1984
    "Brunei Darussalam",
    # 1990
    "Namibia",
    "Lithuania",
    # 1991
    "Estonia", "Georgia", "Latvia", "Belarus", "Ukraine", "Moldova", "Azerbaijan",
    "Uzbekistan", "Kyrgyz Republic", "Tajikistan", "Armenia", "Turkmenistan",
    "Russian Federation", "Kazakhstan", "Slovenia", "Croatia", "North Macedonia",
    # 1992
    "Bosnia and Herzegovina",
    # 1993
    "Eritrea",
    # 1994
    "Palau",
    # 2002
    "Timor-Leste",
    # 2006
    "Serbia", "Montenegro",
    # 2008
    "Kosovo",
    # 2011
    "South Sudan"),
  independence_year = c(1976, 1977, rep(1978,3), rep(1979,3), 1980, rep(1981, 2), 1983, 1984, rep(1990,2), rep(1991,17), 1992, 
           1993, 1994, 2002, rep(2006,2), 2008, 2011))
