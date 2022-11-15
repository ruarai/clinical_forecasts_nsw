
get_constant_estimates <- function(
    nsw_cases,
    hospital_data,
    forecast_dates
) {
  
  source("R/age_groups.R")
  
  fit_period <- c(forecast_dates$hospital_data - days(60), forecast_dates$hospital_data - days(21))
  
  
  
  cases_subset <- nsw_cases %>%
    select(age = AGE_AT_EVENT_YEARS, date_onset = CALCULATED_ONSET_DATE) %>%
    
    filter(date_onset >= fit_period[1], date_onset <= fit_period[2]) %>% 
    mutate(age_group = assign_10yr_age_group(age))
  
  hospital_subset <- hospital_data %>%
    select(age, date_onset = date_onset, ever_in_icu) %>%
    
    filter(date_onset >= fit_period[1], date_onset <= fit_period[2]) %>% 
    mutate(age_group = assign_10yr_age_group(age))
  
  
  n_bootstraps <- 100
  outputs <- list()
  for(i in 1:n_bootstraps) {
    
    total_case_incidence <- cases_subset %>% 
      sample_n(n(), replace = TRUE) %>% 
      drop_na(age_group) %>% 
      count(age_group, name = "n_cases")
    
    hosp_bootstrap <- hospital_subset %>% 
      sample_n(n(), replace = TRUE)
    
    total_hospitalisations <- hosp_bootstrap %>%
      drop_na(age_group) %>% 
      count(age_group, name = "n_hosp")
    
    total_ICU_admissions <- hosp_bootstrap %>%
      filter(ever_in_icu) %>% 
      drop_na(age_group) %>% 
      count(age_group, name = "n_ICU")
    
    prob_estimates <- total_case_incidence %>%
      left_join(total_hospitalisations, by = "age_group") %>%
      left_join(total_ICU_admissions, by = "age_group") %>%
      
      mutate(pr_hosp = n_hosp / n_cases,
             pr_ICU = n_ICU / n_hosp,
             pr_age_given_case = n_cases / sum(n_cases),
             
             pr_hosp = replace_na(pr_hosp, 0),
             pr_ICU = replace_na(pr_ICU, 0),
             pr_age_given_case = replace_na(pr_age_given_case, 0)) %>%
      
      select(age_group, pr_age_given_case, pr_hosp, pr_ICU)
    
    outputs <- c(outputs, list(prob_estimates))
  }
  
  estimates_all <- bind_rows(outputs, .id = "bootstrap")
  
  
  ggplot(estimates_all) +
    geom_jitter(aes(x = age_group, y = pr_hosp))
  ggplot(estimates_all) +
    geom_jitter(aes(x = age_group, y = pr_ICU))
  ggplot(estimates_all) +
    geom_jitter(aes(x = age_group, y = pr_age_given_case))
  
  
  time_notvarying <- estimates_all %>%
    expand_grid(
      date = seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, "days")
    )
  
  time_notvarying
}


