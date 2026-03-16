################################################################################
######################## Simulations for Pre-Trends Test #######################
################################################################################

### Set Seed
set.seed(78261)

### Create Simulations Dataframe
df <- data.frame(
  unit = c(rep(1,10),rep(2,10),rep(3,10),rep(4,10)),
  year = c(2001:2010, 2001:2010, 2001:2010, 2001:2010),
  treatment = c(rep(0,10), rep(0,10), rep(0,5), rep(1,5), rep(0,5), rep(1,5))) %>%
  group_by(unit) %>%
  mutate(
    first_treat_year = ifelse(sum(treatment) > 0, min(year[treatment == 1]), NA),
    time_to_treatment = ifelse(!is.na(first_treat_year), year - first_treat_year, NA)) %>%
  ungroup()

### Create Ever Treated Indicator
df %<>%
  group_by(unit) %>%
  mutate(
    ever_treated = ifelse(sum(treatment) > 0, 1, 0)) %>%
  ungroup()

### Simulate Unit Fixed Effects
unit_fe <- data.frame(
  unit = c(1,2,3,4),
  unit_fe = c(2,6,2,1))

### Simulate Year Fixed Effects
time_fe <- data.frame(
  year = 2001:2010,
  time_fe = c(7,1,7,1,3,2,1,9,10,5))

### Simulate Treatment Effects
df %<>%
  dplyr::mutate(
    treatment_effect = case_when(
      is.na(time_to_treatment) ~ 0,
      time_to_treatment == -5 ~ 5,
      time_to_treatment == -4 ~ 2.5,
      time_to_treatment == -3 ~ -2.5,
      time_to_treatment == -2 ~ -5,
      time_to_treatment == -1 ~ 0,
      time_to_treatment == 0 ~ 5,
      time_to_treatment == 1 ~ 10,
      time_to_treatment == 2 ~ 15,
      time_to_treatment == 3 ~ 20,
      time_to_treatment == 4 ~ 25))

### Merge Dataframe with Unit and Time Fixed Effects
df %<>% left_join(., unit_fe, by = "unit") %>% left_join(., time_fe, by = "year")

### Create Time to Treatment Variable
df$time_to_treatment <- ifelse(is.na(df$time_to_treatment), 0, df$time_to_treatment)

### Simulate Outcomes
df %<>%
  dplyr::mutate(
    outcome = unit_fe + time_fe + treatment_effect + rnorm(n(), mean = 0, sd = 1))

### Run Regression
reg <- feols(outcome ~ i(time_to_treatment, ever_treated, ref = -1) | unit + year, data = df)

### Create Dataframe
coef_df <- broom::tidy(reg)
coef_df$time_to_treatment <- c(-5:-2, 0:4)
coef_df[nrow(coef_df) + 1,] <- list("time_to_treatment::-1:ever_treated", 0, 0, 0, 0, -1)
    
### Plot Event Study Coefficients
ggplot(coef_df, aes(x = time_to_treatment, y = estimate)) +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.2) +
  geom_hline(yintercept = 0, linetype = 1, color = "black") +
  geom_vline(xintercept = -1, linetype = 2, color = "black") +
  scale_x_continuous(breaks = min(coef_df$time_to_treatment):max(coef_df$time_to_treatment)) +
  labs(
    x = "Time to Treatment",
    y = "Event-Study Coefficient") +
  theme_minimal() +
  theme(text = element_text(family = "Times"))

ggsave(filename = "/Users/wilsonking/Desktop/")

df %<>%
  dplyr::mutate(
    time_to_treatment_binned = case_when(
      is.na(time_to_treatment) ~ 0,
      time_to_treatment %in% -5:-2 ~ -2,
      time_to_treatment == -1 ~ -1,
      time_to_treatment %in% 0:1 ~ 0,
      time_to_treatment %in% 2:4 ~ 1
    )
  )


reg <- feols(outcome ~ i(time_to_treatment_binned, ever_treated, ref = -1) | unit + year, data = df)

### Create Dataframe
coef_df <- broom::tidy(reg)
coef_df$time_to_treatment <- c(-2, 0:1)
coef_df[nrow(coef_df) + 1,] <- list("time_to_treatment_binned::-1:ever_treated", 0, 0, 0, 0, -1)

### Plot Event Study Coefficients
ggplot(coef_df, aes(x = time_to_treatment, y = estimate)) +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.2) +
  geom_hline(yintercept = 0, linetype = 1, color = "black") +
  geom_vline(xintercept = -1, linetype = 2, color = "black") +
  scale_x_continuous(breaks = min(coef_df$time_to_treatment):max(coef_df$time_to_treatment)) +
  labs(
    x = "Time to Treatment",
    y = "Event-Study Coefficient") +
  theme_minimal()

