

wos_metadata %<>% 
  dplyr::mutate(
    rice          = as.numeric(grepl("rice", paste(title, abstract, keywords))),
    maize         = as.numeric(grepl("corn|maize", paste(title, abstract, keywords))),
    cassava       = as.numeric(grepl("cassava", paste(title, abstract, keywords))),
    malaria       = as.numeric(grepl("malaria", paste(title, abstract, keywords))),
    yellow_fever  = as.numeric(grepl("yellow fever", paste(title, abstract, keywords))),
    tuberculosis  = as.numeric(grepl("tuberculosis", paste(title, abstract, keywords))),
    female_empowerment = as.numeric(grepl("female empowerment|women's empowerment|empowering women|female economic empowerment|women's economic empowerment")),
    conflict      = as.numeric(grepl("conflict", paste(title, abstract, keywords)))
  )


jpal_ipa_countries <- unique(country_level[country_level$ever_jpal == 1 | country_level$ever_ipa == 1,]$country)


counts <- wos_metadata %>%
  dplyr::filter(rct == 1) %>%
  dplyr::mutate(jpal_ipa_country = ifelse(topic %in% jpal_ipa_countries, 1, 0)) %>%
  group_by(jpal_ipa_country) %>%
  dplyr::summarize(
    n = n(),
    n_rice = sum(rice),
    n_maize = sum(maize),
    n_cassava = sum(cassava),
    n_malaria = sum(malaria),
    n_yellow_fever = sum(yellow_fever),
    n_tuberculosis = sum(tuberculosis),
    n_conflict = sum(conflict))

x <- counts$n_malaria
n <- counts$n  

prop.test(x = x, n = n)


