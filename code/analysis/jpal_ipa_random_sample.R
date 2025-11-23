

set.seed(125125)
random_sample <- wos_data[sample(1:nrow(wos_data), size = 100, replace = FALSE),]
random_sample %<>% dplyr::select(c(authors, title, abstract, journal, keywords, topic, year))
readr::write_csv(x = random_sample, file = paste0(data, "random_sample.csv"))



