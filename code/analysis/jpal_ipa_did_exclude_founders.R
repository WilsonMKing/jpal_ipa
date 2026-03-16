

author_level <- wos_metadata %>%
  dplyr::filter(rct == 1) %>%
  dplyr::mutate(authors = str_remove_all(authors, "\\[|\\]")) %>%           # remove brackets
  dplyr::mutate(authors = str_split(authors, "', '")) %>%                   # split into vector
  dplyr::mutate(authors = map(authors, ~ str_remove_all(.x, "'"))) %>%      # remove trailing quotes
  unnest(authors) %>%
  dplyr::mutate(authors = str_trim(authors)) %>%
  dplyr::mutate(
    last  = word(authors, 1, sep = ", "),
    first = word(authors, 2, sep = ", "),
    first_initial = str_sub(first, 1, 1),
    authors = paste0(last, ", ", first_initial))


### Set Vectors of Treated Countries

  ### J-PAL
  jpal_countries <- 
    c("France", "United States", "India", "South Africa",
      "Egypt, Arab Rep.", "Chile")

  ### IPA
  ipa_countries <- 
    c("Burkina Faso", "Colombia", "Cote d'Ivoire", "Dominican Republic", 
      "Ghana", "Indonesia", "Kenya", "Malawi", "Mali", "Mexico", "Nigeria", "Paraguay", 
      "Peru", "Philippines", "Tanzania", "Uganda", "Zambia", "Rwanda", "Myanmar",
      "Sierra Leone", "Liberia")  

  ### All Treated Countries
  treated_countries <- c(jpal_countries, ipa_countries)
  
  
### Return Author-Level Robustness
author_country_robustness <- function(country){
  
  ### Create Author Shares for Country c
  df <- author_level %>%
    dplyr::filter(topic == !!country) %>%
    group_by(authors) %>%
    dplyr::summarize(n = n()) %>%
    ungroup() %>%
    dplyr::mutate(share = n / n()) %>%
    dplyr::arrange(desc(n))
  
  market_leader_share <- df$share[1]
  market_leader <- df$authors[1]
  hhi <- df %>% dplyr::summarize(HHI = sum(share^2))
  
  return(list(market_leader_share, market_leader, hhi))
  
}


results_df <- data.frame(country = NA, market_leader = NA, market_leader_share = NA, hhi = NA)

for(c in seq_along(treated_countries)){
  
  results <- author_country_robustness(treated_countries[c])
  
  results_df[c,1] <- treated_countries[c]
  results_df[c,2] <- results[[2]]
  results_df[c,3] <- results[[1]]
  results_df[c,4] <- results[[3]]
    
}

results_df %<>%
  dplyr::mutate(
    treatment = case_when(
      country %in% jpal_countries ~ "J-PAL",
      country %in% ipa_countries ~ "IPA"))


ggplot(results_df, aes(x = reorder(country, -hhi), y = hhi, fill = treatment)) +
  geom_col(alpha = 0.7) +
  xlab("") +
  ylab("Herfindahl-Hirschman Index") +
  scale_fill_manual(
    values = c("J-PAL" = "orange", "IPA" = "darkgreen")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        text = element_text(family = "Times"))

ggplot(results_df, aes(x = reorder(country, -market_leader_share), y = market_leader_share, fill = treatment)) +
  geom_col(alpha = 0.7) +
  xlab("") +`
  ylab("Market Leader Share of RCTs") +
  scale_fill_manual(
    values = c("J-PAL" = "orange", "IPA" = "darkgreen")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        text = element_text(family = "Times"))
  
market_leaders_list <- results_df$market_leader

################################################################################

freqs_panel_no_founders <- wos_metadata %>%
  dplyr::filter(
    !stringr::str_detect(authors, "Duflo, E"), 
    !stringr::str_detect(authors, "Banerjee, A"),
    !stringr::str_detect(authors, "Mullainathan, S"),
    !stringr::str_detect(authors, "Karlan, D")) %>%
  create_freqs_panel(.) %>%
  dplyr::filter(year >= 1995 & year <= 2024)

freqs_panel_no_founders_affiliates <- wos_metadata %>%
  dplyr::filter(
    !stringr::str_detect(authors, "Duflo, E"), 
    !stringr::str_detect(authors, "Banerjee, A"),
    !stringr::str_detect(authors, "Mullainathan, S"),
    !stringr::str_detect(authors, "Karlan, D"),
    !stringr::str_detect(authors, "Bertrand, M"),
    !stringr::str_detect(authors, "Kremer, M"),
    !stringr::str_detect(authors, "Levy, D"),
    !stringr::str_detect(authors, "Miguel, E")) %>%
  create_freqs_panel(.) %>%
  dplyr::filter(year >= 1995 & year <= 2024)

market_leaders_pattern <- paste(market_leaders_list, collapse = "|")
freqs_panel_no_market_leaders <- wos_metadata %>%
  dplyr::filter(!grepl(market_leaders_pattern, authors)) %>%
  create_freqs_panel() %>%
  dplyr::filter(year >= 1995 & year <= 2024)



### Run Regressions

### Number of RCTs
reg1 <- run_ipa_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel_no_founders)
reg2 <- run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel_no_founders)
reg3 <- run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did-both", data = freqs_panel_no_founders)

### Number of RCTs
reg4 <- run_ipa_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel_no_founders_affiliates)
reg5 <- run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel_no_founders_affiliates)
reg6 <- run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did-both", data = freqs_panel_no_founders_affiliates)

### Number of RCTs
reg7 <- run_ipa_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel_no_market_leaders)
reg8 <- run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel_no_market_leaders)
reg9 <- run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did-both", data = freqs_panel_no_market_leaders)

### Create Table
table <- stargazer::stargazer(
  reg1, reg2, reg3,
  reg4, reg5, reg6,
  reg7, reg8, reg9,
  align = TRUE,
  header = FALSE,
  digits = 2,
  float = FALSE,
  omit = c("Constant", "population", "gdp"),
  covariate.labels = c("IPA Office", "J-PAL Office"),
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  omit.stat = c("rsq", "adj.rsq", "ser"),
  omit.table.layout = "n") %>%
  starpolishr::star_insert_row(
    c(" & \\multicolumn{9}{c}{Number of RCTs} \\\\",
      "\\cmidrule(lr){2-10}",
      "& \\multicolumn{3}{c}{Excluding Founders} & \\multicolumn{3}{c}{Exclude Founders \\& Founding Affiliates} & \\multicolumn{3}{c}{Exclude Market Leaders}\\\\",
      "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10}",
      paste0(
        "Control Mean & ", 
        round(sum(freqs_panel_no_founders[freqs_panel_no_founders$ever_ipa == 0,]$frequency_rct) / nrow(freqs_panel_no_founders[freqs_panel_no_founders$ever_ipa == 0,]),2), " & ",
        round(sum(freqs_panel_no_founders[freqs_panel_no_founders$ever_jpal == 0,]$frequency_rct) / nrow(freqs_panel_no_founders[freqs_panel_no_founders$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel_no_founders[freqs_panel_no_founders$ever_ipa == 0 & freqs_panel_no_founders$ever_jpal == 0,]$frequency_rct) / nrow(freqs_panel_no_founders[freqs_panel_no_founders$ever_ipa == 0 & freqs_panel_no_founders$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel_no_founders_affiliates[freqs_panel_no_founders_affiliates$ever_ipa == 0,]$frequency_rct) / nrow(freqs_panel_no_founders_affiliates[freqs_panel_no_founders_affiliates$ever_ipa == 0,]),2), " & ",
        round(sum(freqs_panel_no_founders_affiliates[freqs_panel_no_founders_affiliates$ever_jpal == 0,]$frequency_rct) / nrow(freqs_panel_no_founders_affiliates[freqs_panel_no_founders_affiliates$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel_no_founders_affiliates[freqs_panel_no_founders_affiliates$ever_ipa == 0 & freqs_panel_no_founders_affiliates$ever_jpal == 0,]$frequency_rct) / nrow(freqs_panel_no_founders_affiliates[freqs_panel_no_founders_affiliates$ever_ipa == 0 & freqs_panel_no_founders_affiliates$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel_no_market_leaders[freqs_panel_no_market_leaders$ever_ipa == 0,]$frequency_rct) / nrow(freqs_panel_no_market_leaders[freqs_panel_no_market_leaders$ever_ipa == 0,]),2), " & ",
        round(sum(freqs_panel_no_market_leaders[freqs_panel_no_market_leaders$ever_jpal == 0,]$frequency_rct) / nrow(freqs_panel_no_market_leaders[freqs_panel_no_market_leaders$ever_jpal == 0,]),2), " & ",
        round(sum(freqs_panel_no_market_leaders[freqs_panel_no_market_leaders$ever_ipa == 0 & freqs_panel_no_market_leaders$ever_jpal == 0,]$frequency_rct) / nrow(freqs_panel_no_market_leaders[freqs_panel_no_market_leaders$ever_ipa == 0 & freqs_panel_no_market_leaders$ever_jpal == 0,]),2), " \\\\"),
      "Controls & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\\\"),
    insert.after = c(4,4,4,4,14,14))
table

### Save
starpolishr::star_tex_write(table, file = paste0(exhibits, "tables/jpal_ipa_did_exclude_founders.tex"))

################################################################################

### Number of RCTs
ipa <- run_ipa_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel)
jpal <- run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel)

### Number of RCTs
ipa_no_founders <- run_ipa_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel_no_founders)
jpal_no_founders <- run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel_no_founders)

### Number of RCTs
ipa_no_founders_affiliates <- run_ipa_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel_no_founders_affiliates)
jpal_no_founders_affiliates <- run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel_no_founders_affiliates)

### Number of RCTs
ipa_no_market_leaders <- run_ipa_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel_no_market_leaders)
jpal_no_market_leaders <- run_jpal_regressions(outcome = "frequency_rct", specification = "ols-binary-did", data = freqs_panel_no_market_leaders)


df <- data.frame(
  coef = c(ipa$coefficients[1], ipa_no_founders$coefficients[1], ipa_no_founders_affiliates$coefficients[1], ipa_no_market_leaders$coefficients[1], 
           jpal$coefficients[1], jpal_no_founders$coefficients[1], jpal_no_founders_affiliates$coefficients[1], jpal_no_market_leaders$coefficients[1]),
  se = c(ipa$cse[1], ipa_no_founders$cse[1], ipa_no_founders_affiliates$cse[1], ipa_no_market_leaders$cse[1], 
           jpal$cse[1], jpal_no_founders$cse[1], jpal_no_founders_affiliates$cse[1], jpal_no_market_leaders$cse[1]),
  pval = c(ipa$cpval[1], ipa_no_founders$cpval[1], ipa_no_founders_affiliates$cpval[1], ipa_no_market_leaders$cpval[1], 
           jpal$cpval[1], jpal_no_founders$cpval[1], jpal_no_founders_affiliates$cpval[1], jpal_no_market_leaders$cpval[1]),
  institution = c("IPA", "IPA", "IPA", "IPA", "J-PAL", "J-PAL", "J-PAL", "J-PAL"),
  dropping = c("Main", "Founders", "Founders+Affiliates", "Market Leaders")
)


df$dropping <- factor(df$dropping, 
                      levels = rev(c("Main", 
                                     "Founders", 
                                     "Founders+Affiliates", 
                                     "Market Leaders")))

df$stars <- cut(df$pval,
                breaks = c(-Inf, 0.01, 0.05, 0.1, Inf),
                labels = c("***", "**", "*", ""),
                right = FALSE)

df_error <- df[df$dropping == "Main Results", ]

ggplot(df, aes(x = dropping, y = coef, fill = institution, color = institution)) +
  geom_col(alpha = 0.7, position = position_dodge(width = 0.9)) +
  geom_errorbar(data = df_error,
                aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(label = stars, y = coef + 0.1*coef),
            position = position_dodge(width = 0.9),
            size = 8,
            vjust = 0) +
  scale_color_manual(values = c("J-PAL" = "orange", "IPA" = "darkgreen")) +
  scale_fill_manual(values = c("J-PAL" = "orange", "IPA" = "darkgreen")) +
  ylab("") + xlab("") +
  theme_classic() +
  theme(
    text = element_text(family = "Times"),
    legend.position = "none",
    strip.text = element_blank()
  ) +
  coord_flip() +
  facet_wrap(~ institution, scales = "free_x")



ggsave(plot = last_plot(), paste0(exhibits, "jpal_dropping_founders.jpeg"), units = "cm",
       width = 16, height = 10)

