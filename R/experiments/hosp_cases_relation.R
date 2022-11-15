
nsw_cases <- tar_read(nsw_cases)
hospital_data <- tar_read(hospital_data_unfiltered)

source("R/age_groups.R")

case_counts <- nsw_cases %>% 
  mutate(age_group = assign_10yr_age_group(AGE_AT_EVENT_YEARS)) %>%
  drop_na(age_group) %>%
  count(age_group, date_onset = CALCULATED_ONSET_DATE,
        name = "n_cases") %>%
  filter(date_onset >= ymd("2022-03-01"), date_onset <= ymd("2022-11-01")) %>%
  
  complete(age_group = age_groups, date_onset = seq(ymd("2022-03-01"), ymd("2022-11-01"), "days"),
           fill = list(n_cases = 0))


hospital_subset <- hospital_data %>%
  select(age, date_onset = date_onset, ever_in_icu) %>%
  
  mutate(age_group = assign_10yr_age_group(age))


hospital_counts <- hospital_subset %>%
  count(date_onset, age_group, name = "n_hosp") %>%
  complete(date_onset, age_group, fill = list(n_hosp = 0))

ggplot(case_counts) +
  
  geom_line(aes(x = date_onset, y = n_cases)) +
  
  facet_wrap(~age_group)

time_varying_estimates <- tar_read(time_varying_estimates)


join <- time_varying_estimates %>%
  group_by(date, age_group) %>%
  summarise(pr_hosp = mean(pr_hosp)) %>%
  
  left_join(case_counts %>% rename(date = date_onset)) %>%
  
  drop_na(n_cases)

join %>% 
  mutate(
    period = case_when(
      date <= ymd("2022-02-15") ~ "BA.1",
      date <= ymd("2022-05-15") ~ "BA.2",
      date <= ymd("2022-10-01") ~ "BA.5",
      TRUE ~ "current"
    )
  ) %>% 
  
  #group_by(age_group) %>%
  #mutate(pr_hosp = pr_hosp - min(pr_hosp)) %>% 
  
  ggplot() +
  
  geom_point(aes(x = n_cases, y = pr_hosp, colour = period),
             size = 0.4) +
  
  facet_wrap(~age_group,
             
             scales = "free") +
  
  theme_minimal()

fit_data <- join %>%
  mutate(log_n_cases = log(n_cases),
         inv_cases = 1 / n_cases,
         n_hosp = pr_hosp * n_cases,
         logit_pr_hosp = qlogis(pr_hosp),
         age_group = factor(age_group)) %>%
  ungroup()


fit <- lm(n_hosp ~ n_cases * age_group, data = fit_data)


fit <- lm(logit_pr_hosp ~ log_n_cases * age_group + inv_cases * age_group, data = fit_data)


library(mgcv)
gam_fit <- gam(logit_pr_hosp ~ inv_cases * age_group, data = fit_data)
fit <- gam(logit_pr_hosp ~ s(log_n_cases, by = age_group, k = 3) + age_group, data = fit_data)
summary(gam_fit)



fit_data %>%
  ungroup() %>%
  mutate(pred_logit_pr_hosp = predict(fit, newdata = .))  %>% 
  
  ggplot() +
  geom_point(aes(x = date, y = logit_pr_hosp), size = 0.3) +
  
  geom_line(aes(x = date, y = pred_logit_pr_hosp), colour = ggokabeito::palette_okabe_ito(5)) +
  facet_wrap(~age_group, scales = "free_y") +
  
  
  theme_minimal()

fit_data %>%
  ungroup() %>%
  mutate(pred_logit_pr_hosp = predict(fit, newdata = .),
         pred_pr_hosp = plogis(pred_logit_pr_hosp),
         pred_n_hosp = n_cases * pred_pr_hosp)  %>% 
  
  ggplot() +
  
  geom_point(aes(x = n_cases, y = n_hosp),
             size = 0.4) +
  
  geom_line(aes(x = n_cases, y = pred_n_hosp)) +
  
  facet_wrap(~age_group,
             
             scales = "free") +
  
  theme_minimal()


case_counts %>%
  left_join(hospital_counts) %>%
  
  ggplot() +
  geom_line(aes(x = date, y = n_hosp)) +
  facet_wrap(~age_group)

  mutate(pr_hosp = n_hosp / n_cases) %>%
  
  ggplot() +
  geom_point(aes(x = log(n_cases), y = pr_hosp)) +
  
  facet_wrap(~age_group, scales = "free") +
  theme_minimal()

