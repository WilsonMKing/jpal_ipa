




wos_metadata %>%
  group_by(journal) %>%
  dplyr::rename(Journal = journal) %>%
  dplyr::summarize(`Number of RCTs` = round(sum(rct) / n(), 2)) %>%
  arrange(desc(`Number of RCTs`)) %>%
  slice(1:15) %>%
  stargazer::stargazer(summary = FALSE)
