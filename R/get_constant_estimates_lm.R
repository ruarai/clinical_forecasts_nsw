
get_constant_estimates_lm <- function(
  nsw_cases,
  hospital_data,
  case_trajectory,
  time_varying_estimates_moving,
  
  forecast_dates
) {
  
  source("R/age_groups.R")
  
  require(mgcv)
  
  fit_period <- c(ymd("2022-06-01"), forecast_dates$hospital_data - days(14))
  
  
  
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
      dow = wday(date_onset),
      
      weight = 1,
      
      .before = 2
    )
  
  
  
  hosp_fits <- map(
    age_groups,
    function(i_age_group) {
      gam(
        #n_hosp ~ s(t, by = n_cases, k = 10) + s(dow, k = 5, by = n_cases),
        n_hosp ~ n_cases,
        data = fit_data %>% filter(age_group == i_age_group),
        
        weights = weight
      )
    }
  ) %>%
    `names<-`(age_groups)
  
  
  
  
  moving_estimates_with_cases <- time_varying_estimates_moving %>%
    left_join(case_trajectory %>% select(date = date_onset, n_cases = count)) %>%
    mutate(n_cases = n_cases * pr_age_given_case)
  
  
  time_varying_estimates_hosp <- map_dfr(
    age_groups,
    function(i_age_group) {
      moving_estimates_with_cases %>%
        filter(age_group == i_age_group) %>%
        mutate(t = as.numeric(date - fit_period[1]),
               dow = wday(date),
               t = if_else(date >= fit_period[2], as.numeric(fit_period[2] - fit_period[1]), t)) %>%
        mutate(pred_n_hosp = predict(hosp_fits[[i_age_group]], newdata = .),
               pr_hosp = pred_n_hosp / n_cases,
               pr_hosp = pmin(pmax(pr_hosp, 0), 1)) %>%
        select(bootstrap, age_group, date, pr_age_given_case, pr_hosp, pr_ICU)
    }
  )
  
  
  time_varying_estimates_hosp
}


