run_progression_model <- function(
  time_varying_estimates,
  case_trajectory,
  clinical_parameter_samples,
  
  forecast_dates,
  
  do_ABC = FALSE
) {
  
  estimates_to_matrix <- function(x, variable) {
    col <- deparse(substitute(variable))
    
    x %>%
      select(date, bootstrap, age_group, all_of(col)) %>%
      pivot_wider(names_from = bootstrap,
                  values_from = all_of(col)) %>%
      
      arrange(date, age_group) %>%
      select(-c(date, age_group)) %>%
      as.matrix()
  }
  
  mat_pr_age_given_case <- time_varying_estimates %>%
    group_by(date, bootstrap) %>%
    mutate(pr_age_given_case = pr_age_given_case / sum(pr_age_given_case)) %>%
    ungroup() %>%
    estimates_to_matrix(pr_age_given_case)
  
  
  mat_pr_hosp <- time_varying_estimates %>%
    estimates_to_matrix(pr_hosp)
  
  mat_pr_ICU <- time_varying_estimates %>%
    estimates_to_matrix(pr_ICU)
  
  case_matrix <- case_trajectory %>%
    arrange(date_onset) %>%
    pull(count) %>%
    as.matrix()
  
  n_days <- nrow(case_matrix)

  clinical_parameter_samples <- clinical_parameter_samples %>%
    mutate(shape_onset_to_ward = shape_onset_to_ward * 0.7,
           scale_onset_to_ward = scale_onset_to_ward * 0.7)
  
  
  occupancy_curve_match <- tibble(
    date = seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, by = 'days')
  ) %>%
    mutate(do_match = date >= forecast_dates$forecast_start - ddays(7) & date <= forecast_dates$forecast_start) %>%
    
    left_join(
      
      tar_read(known_occupancy_ts_NSW, store = "../clinical_forecasting/_targets") %>%
        filter(source == "c19", state == "NSW") %>%
        
        filter(date >= forecast_dates$simulation_start) %>%
        select(date, group, count) %>%
        pivot_wider(names_from = group, values_from = count)
      
    ) %>%
    
    mutate(ward = zoo::rollmean(ward, 7, align = "r", fill = NA),
           ICU = zoo::rollmean(ICU, 7, align = "r", fill = NA),
           ward_vec = if_else(do_match, ward, -1),
           ICU_vec = if_else(do_match, ICU, -1),
           
           ward_vec = replace_na(ward_vec, -1),
           ICU_vec = replace_na(ICU_vec, -1))
  
  prior_sigma_los <- if_else(do_ABC, 0.8, 0)
  prior_sigma_hosp <- if_else(do_ABC, 0.8, 0)
  
  a <- Sys.time()
  thresholds <- c(0.1, 0.2, 0.3, 0.4, 0.5, 10)
  results <- curvemush::mush_abc(
    n_samples = 4000,
    n_delay_samples = 512,
    
    n_outputs = 1000,
    
    n_days = n_days,
    steps_per_day = 4,
    
    do_ABC = do_ABC,
    thresholds = thresholds,
    rejections_per_selections = if_else(do_ABC, 500, 1),
    
    prior_sigma_los = prior_sigma_los,
    prior_sigma_hosp = prior_sigma_hosp,
    
    ensemble_curves = case_matrix,
    
    forecasting_parameters = clinical_parameter_samples,
    
    known_ward_vec = occupancy_curve_match$ward_vec,
    known_ICU_vec = occupancy_curve_match$ICU_vec,
    
    mat_pr_age_given_case = mat_pr_age_given_case,
    mat_pr_hosp = mat_pr_hosp,
    mat_pr_ICU = mat_pr_ICU
  )
  
  b <- Sys.time()
  print(b - a)
  
  
  source("../clinical_forecasting/R/progression_model.R")
  group_labels <- c("symptomatic_clinical", "ward", "ICU", "discharged", "died")
  compartment_labels <- c(
    "symptomatic_clinical", "ward", "discharged_ward", "died_ward", "ICU",
    "discharged_ICU", "died_ICU", "postICU_to_discharge", "postICU_to_death",
    "discharged_postICU", "died_postICU"
  )
  
  format_grouped <- . %>%
    mutate(date = forecast_dates$simulation_start + ddays(t_day),
           group = group_labels[compartment_group + 1])
  
  format_ungrouped <- . %>%
    mutate(date = forecast_dates$simulation_start + ddays(t_day),
           compartment = compartment_labels[compartment + 1])
  
  
  
  results_count_quants <- results$grouped_results %>%
    select(-c(transitions)) %>%
    pivot_wider(names_from = "sample",
                names_prefix = "sim_",
                values_from = "count") %>%
    make_results_quants() %>%
    format_grouped()
  
  results_ungrouped_count_quants <- results$results %>%
    select(-c(transitions)) %>%
    pivot_wider(names_from = "sample",
                names_prefix = "sim_",
                values_from = "count") %>%
    make_results_quants() %>%
    format_ungrouped()
  
  results_transitions_quants <- results$grouped_results %>%
    select(-c(count)) %>%
    pivot_wider(names_from = "sample",
                names_prefix = "sim_",
                values_from = "transitions") %>%
    make_results_quants() %>%
    format_grouped()
  
  results_ungrouped_transitions_quants <- results$results %>%
    select(-c(count)) %>%
    pivot_wider(names_from = "sample",
                names_prefix = "sim_",
                values_from = "transitions") %>%
    make_results_quants() %>%
    format_ungrouped()
  
  
  age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  
  
  results_aged_quants <- results$age_stratified_grouped_results %>%
    select(-c(transitions)) %>%
    pivot_wider(names_from = "sample",
                names_prefix = "sim_",
                values_from = "count") %>%
    make_results_quants() %>%
    format_grouped() %>%
    mutate(age_group = age_groups[age_group + 1])
  
  
  results_aged_transitions_quants <- results$age_stratified_grouped_results %>%
    select(-c(count)) %>%
    pivot_wider(names_from = "sample",
                names_prefix = "sim_",
                values_from = "transitions") %>%
    make_results_quants() %>%
    format_grouped() %>%
    mutate(age_group = age_groups[age_group + 1])
  
  
  results_formatted <- results$grouped_results  %>%
    format_grouped()
  
  results_ungrouped_formatted <- results$results  %>%
    format_ungrouped()
  
  results_aged_ungrouped_formatted <- results$results  %>%
    format_ungrouped()
  
  
  posterior_data <- tibble(
    pr_hosp_scale = results$prior_pr_hosp[results$prior_chosen + 1],
    los_scale = results$prior_los_scale[results$prior_chosen + 1],
    source = "posterior"
  )
  
  prior_data <- tibble(
    los_scale = rnorm(1000, sd = prior_sigma_los),
    pr_hosp_scale = rnorm(1000, sd = prior_sigma_hosp),
    source = "prior"
  )
  
  ABC_parameters <- bind_rows(posterior_data, prior_data)
  
  list(
    results_formatted = results_formatted,
    results_ungrouped_formatted = results_ungrouped_formatted,
    results_count_quants = results_count_quants,
    results_ungrouped_count_quants = results_ungrouped_count_quants,
    results_ungrouped_transitions_quants = results_ungrouped_transitions_quants,
    
    results_aged_quants = results_aged_quants,
    results_aged_transitions_quants = results_aged_transitions_quants,
    
    ABC_fit_diagnostics = tibble(thresholds = thresholds, accepted = results$n_accepted),
    ABC_parameters = ABC_parameters
  )

}

