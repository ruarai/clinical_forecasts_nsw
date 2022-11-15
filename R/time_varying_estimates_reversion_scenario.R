adjust_estimates_scenario <- function(
    time_varying_estimates,
    forecast_dates
) {
  time_varying_subset <- time_varying_estimates %>%
    
    mutate(
      pr_hosp = if_else(date >= forecast_dates$hospital_data - days(7), NA_real_, pr_hosp),
      pr_ICU = if_else(date >= forecast_dates$hospital_data - days(7), NA_real_, pr_ICU)
    )  %>%
    
    drop_na(pr_hosp)
  
  reversion_date <- ymd("2022-07-01")
  
  bind_rows(
    time_varying_subset,
    time_varying_subset %>% filter(date == reversion_date) %>%
      mutate(date = forecast_dates$forecast_start + days(14),
             pr_age_given_case = NA_real_),
    time_varying_subset %>% filter(date == reversion_date) %>% 
      mutate(date = forecast_dates$forecast_horizon,
             pr_age_given_case = NA_real_)
  ) %>%
    
    complete(
      date = seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, "days"),
      age_group,
      bootstrap
    ) %>%
    
    arrange(date) %>%
    group_by(bootstrap, age_group) %>%
    
    mutate(
      pr_hosp = zoo::na.approx(pr_hosp),
      pr_ICU = zoo::na.approx(pr_ICU)
    ) %>%
    fill(pr_age_given_case) %>%
    ungroup()
}


