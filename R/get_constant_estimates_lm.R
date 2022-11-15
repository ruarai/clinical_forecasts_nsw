
get_constant_estimates_lm <- function(
  nsw_cases,
  hospital_data,
  case_trajectory,
  
  forecast_dates
) {
  
  source("R/age_groups.R")
  
  require(mgcv)
  
  fit_period <- c(ymd("2022-02-01"), forecast_dates$hospital_data - days(14))
  
  
  
  cases_subset <- nsw_cases %>%
    select(age = AGE_AT_EVENT_YEARS, date_onset = CALCULATED_ONSET_DATE) %>%
    
    filter(date_onset >= fit_period[1], date_onset <= fit_period[2]) %>% 
    mutate(age_group = assign_10yr_age_group(age))
  
  hospital_subset <- hospital_data %>%
    select(age, date_onset = date_onset, ever_in_icu) %>%
    
    filter(date_onset >= fit_period[1], date_onset <= fit_period[2]) %>% 
    mutate(age_group = assign_10yr_age_group(age))
  
  
  
  
  case_count <- cases_subset %>%
    count(date_onset, age_group, name = "n_cases") %>%
    complete(
      date_onset = seq(fit_period[1], fit_period[2], "days"),
      age_group = age_groups,
      fill = list(n_cases = 0)
    )
  
  hospital_count <- hospital_subset %>%
    count(date_onset, age_group, name = "n_hosp") %>%
    complete(
      date_onset = seq(fit_period[1], fit_period[2], "days"),
      age_group = age_groups,
      fill = list(n_hosp = 0)
    )
  
  ICU_count <- hospital_subset %>%
    filter(ever_in_icu) %>%
    count(date_onset, age_group, name = "n_ICU") %>%
    complete(
      date_onset = seq(fit_period[1], fit_period[2], "days"),
      age_group = age_groups,
      fill = list(n_ICU = 0)
    )
  
  
  fit_data <- case_count %>%
    left_join(hospital_count, by = c("date_onset", "age_group")) %>%
    left_join(ICU_count, by = c("date_onset", "age_group")) %>%
    
    drop_na(age_group) %>%
    
    mutate(
      age_group = factor(age_group),
      t = as.numeric(date_onset - fit_period[1]),
      .before = 2
    )
  
  
  hosp_fits <- map(
    age_groups,
    function(i_age_group) {
      gam(
        #n_hosp ~ s(t, by = n_cases, k = 10),
        n_hosp ~ n_cases,
        data = fit_data %>% filter(age_group == i_age_group)
      )
    }
  ) %>%
    `names<-`(age_groups)
  
  
  total_hospitalisations <- hospital_subset %>%
    drop_na(age_group) %>% 
    count(age_group, name = "n_hosp")
  
  total_ICU_admissions <- hospital_subset %>%
    filter(ever_in_icu) %>% 
    drop_na(age_group) %>% 
    count(age_group, name = "n_ICU")
  
  prob_ICU <- total_hospitalisations %>%
    left_join(total_ICU_admissions, by = "age_group") %>%
    
    mutate(pr_ICU = n_ICU / n_hosp,
           
           pr_ICU = replace_na(pr_ICU, 0)) %>%
    
    select(age_group, pr_ICU)
  
  
  
  
  
  
  
  cases_fit <- gam(
    n_cases ~ s(t, k = 15, by = age_group) + age_group,
    
    family = nb,
    data = fit_data
  )
  
  
  fit_data %>%
    mutate(pred_n_cases = predict(cases_fit, newdata = .),
           pred_n_cases = exp(pred_n_cases)) %>%
    
    ggplot() +
    
    geom_point(aes(x = date_onset, y = n_cases),
               size = 0.4) +
    geom_line(aes(x = date_onset, y = pred_n_cases),
              size = 1,
              colour = ggokabeito::palette_okabe_ito(5)) +
    
    facet_wrap(~age_group, scales = "free_y") +
    
    theme_minimal()
  
  
  fit_data %>%
    mutate(pred_n_cases = predict(cases_fit, newdata = .),
           pred_n_cases = exp(pred_n_cases)) %>%
    
    group_by(date_onset) %>% 
    mutate(
      pred_pr_age_given_case = pred_n_cases / sum(pred_n_cases),
      pr_age_given_case = n_cases / sum(n_cases)
    ) %>% 
    
    ggplot() +
    
    geom_point(aes(x = date_onset, y = pr_age_given_case),
               size = 0.4) +
    geom_line(aes(x = date_onset, y = pred_pr_age_given_case),
              size = 1,
              colour = ggokabeito::palette_okabe_ito(5)) +
    
    facet_wrap(~age_group, scales = "free_y") +
    
    theme_minimal()
  
  
  
  
  time_varying_estimates <- fit_data %>%
    select(date_onset, t, age_group) %>% 
    
    mutate(
      pred_n_cases = predict(cases_fit, newdata = .),
      pred_n_cases = exp(pred_n_cases)
    ) %>%
    group_by(date_onset) %>%
    mutate(pr_age_given_case = pred_n_cases / sum(pred_n_cases)) %>%
    ungroup() %>%
    select(date_onset, age_group, pr_age_given_case) %>%
    
    
    
    left_join(prob_ICU) %>%
    select(date = date_onset, age_group, pr_age_given_case, pr_ICU) %>%
    ungroup() %>%
    
    complete(
      age_group = age_groups,
      date = seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, by = "days")
    ) %>%
    group_by(age_group) %>%
    arrange(date) %>%
    fill(pr_age_given_case, pr_ICU, .direction = "downup") %>%
    ungroup() %>%
    
    left_join(case_trajectory %>% rename(n_cases = count, date = date_onset), by = "date") %>% 
    
    mutate(
      n_cases = n_cases * pr_age_given_case
    )
  
  time_varying_estimates_hosp <- map_dfr(
    age_groups,
    function(i_age_group) {
      time_varying_estimates %>%
        filter(age_group == i_age_group) %>%
        mutate(t = as.numeric(date - fit_period[1])) %>% 
        mutate(pred_n_hosp = predict(hosp_fits[[i_age_group]], newdata = .),
               pr_hosp = pred_n_hosp / n_cases,
               pr_hosp = pmin(pmax(pr_hosp, 0), 1)) %>%
        select(age_group, date, pr_age_given_case, pr_hosp, pr_ICU)
    }
  ) %>%
    mutate(bootstrap = 1)
  
  
  time_varying_estimates_hosp
}


