

adjust_morbidity_trajectories <- function(
  is_longterm,
  immune_predictions_state,
  morbidity_trajectories_state,
  state_forecast_start
) {
  if(!is_longterm) {
    return(morbidity_trajectories_state)
  }
  
  immune_predictions_complete <- immune_predictions_state %>%
    complete(date = seq(min(morbidity_trajectories_state$date),
                        max(morbidity_trajectories_state$date),
                        by = "day"),
             age_group,
             sim) %>%
    arrange(date) %>%
    
    group_by(age_group, sim) %>%
    
    mutate(across(c(m_hosp, pr_age_given_case), ~ zoo::na.approx(., rule = 2)))
  
  
  n_bootstraps <- max(morbidity_trajectories_state$bootstrap)
  
  incidental_rate <- 0.5
  
  adjusted_estimates_state <- morbidity_trajectories_state %>%
    rename(pr_age_old = pr_age_given_case) %>%
    
    left_join(
      immune_predictions_complete %>%
        ungroup() %>%
        select(-sim),
      
      by = c("date", "age_group")
    ) %>%
    
    mutate(pr_hosp_incidental = pr_hosp * incidental_rate,
           pr_hosp_direct = pr_hosp * (1 - incidental_rate)) %>%
    
    mutate(pr_hosp_direct = if_else(date > state_forecast_start, NA_real_, pr_hosp_direct)) %>%
    
    mutate(x = pr_hosp_direct / m_hosp)  %>%
    
    arrange(date) %>%
    group_by(bootstrap, age_group) %>%
    fill(x, .direction = "down") %>%
    ungroup() %>%
    
    mutate(pred_pr_hosp = pr_hosp_incidental + x * m_hosp) %>%
    
    
    select(bootstrap, date, age_group, pr_age_given_case, pr_hosp = pred_pr_hosp, pr_ICU, pr_hosp_old = pr_hosp, pr_age_old)
  
}
