
library(targets)


ED_counts <- ED_linelist %>%
  #filter(covidlike) %>%
  mutate(present_after_onset = presentation_date > ncims_calc_onset_dt) %>% 
  count(presentation_date = as_date(presentation_date), age_group, present_after_onset)


case_data <- tar_read(nsw_cases)


case_counts <- case_data %>%
  filter(TEST_TYPE == "PCR") %>% 
  mutate(age_group = assign_10yr_age_group(AGE_AT_EVENT_YEARS)) %>% 
  count(date_onset = CALCULATED_ONSET_DATE, age_group) %>%
  
  drop_na(age_group) %>%
  filter(date_onset >= ymd("2021-07-01"))


data <- ED_counts %>%
  rename(n_present = n, date = presentation_date) %>%
  left_join(
    case_counts %>%
      rename(n_case = n,
             date = date_onset) %>%
      mutate(date = date - days(4))
  ) %>%
  
  complete(
    date = seq(min(date), max(date), "days"),
    age_group = age_groups,
    present_after_onset = c(FALSE, TRUE),
    fill = list(n_case = 0, n_present = 0)
  ) %>%
  
  filter(date >= ymd("2022-01-01"),
         date <= ymd("2022-10-05")) %>%
  
  mutate(t = as.numeric(date - ymd("2020-01-01")),
         age_group = factor(age_group),
         
         f = interaction(age_group, present_after_onset),
         
         n_present = pmin(n_present, n_case))

library(mgcv)

fit <- bam(
  cbind(n_present, n_case - n_present) ~ s(t, by = f, k = 30) + f,
  
  
  family = "binomial",
  data = data,
  discrete = TRUE
)


plot_data <- expand_grid(
  date = seq(ymd("2022-01-01"), ymd("2022-10-01"), "days"),
  age_group = age_groups,
  present_after_onset = c(TRUE, FALSE)
)  %>%
  
  mutate(t = as.numeric(date - ymd("2020-01-01")),
         f = interaction(age_group, present_after_onset)) %>%
  
  mutate(p_present = predict(fit, newdata = ., type = "response"))


data %>%
  ggplot() +
  geom_point(aes(x = date, y = n_present / n_case, colour = present_after_onset),
             size = 0.2) +
  
  geom_line(aes(x = date, y = p_present, colour = present_after_onset),
            size = 0.8,
            plot_data) +
  
  facet_wrap(~age_group, scales = "free_y") + 
  
  scale_y_continuous(trans = "logit") +
  
  ggokabeito::scale_colour_okabe_ito() +
  
  plot_theme




plot_data <- data %>%
  drop_na(age_group) %>%
  filter(date >= ymd("2022-04-01")) %>%
  select(-f) %>%
  pivot_wider(values_from = n_present,
              names_from = present_after_onset,
              names_prefix = "after_")

ggplot(plot_data) +
  # geom_linerange(aes(x = date, ymin = 0, ymax = n_case),
  #                size = 0.9) +
  geom_point(aes(x = date, y = after_FALSE / n_case),
             colour = "red",
             size = 0.9) +
  geom_point(aes(x = date, y = after_TRUE / n_case),
             colour = "blue",
             size = 0.9) +
  #scale_y_log10() +
  
  facet_wrap(~age_group) +
  
  plot_theme
