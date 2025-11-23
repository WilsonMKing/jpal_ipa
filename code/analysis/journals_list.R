################################################################################
################################################################################
################################################################################


wos_metadata %>%
  group_by(journal) %>%
  dplyr::summarize(`Number of Articles` = n()) %>%
  dplyr::rename(Journal = journal) %>%
  stargazer::stargazer(summary = FALSE, header = FALSE, float = FALSE) %>%
  star_tex_write(., file = paste0(exhibits, "journals_list.tex"))
