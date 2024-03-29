run_progression_model <- function(
  time_varying_estimates,
  case_trajectory,
  clinical_parameter_samples,
  
  occupancy_data,
  
  forecast_dates,
  
  do_ABC = FALSE
) {
  
  # Transform a variable from the time-varying estimates into a matrix that can be read by the model
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
  
  
  # Transform the case trajectory into a single-column matrix for the model
  # If multiple trajectory need to be provided, this will need updating
  case_matrix <- case_trajectory %>%
    arrange(date_onset) %>%
    pull(count) %>%
    as.matrix()
  
  n_days <- nrow(case_matrix)
  
  # Produce two vectors (for ward and ICU) of known occupancy to be fit to
  occupancy_curve_match <- tibble(
    date = seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, by = 'days')
  ) %>%
    mutate(do_match = date >= forecast_dates$forecast_start - ddays(7) & date <= forecast_dates$forecast_start) %>% 
    # mutate(
    #   days_ago = as.numeric(forecast_dates$forecast_start - date),
    #   do_match = days_ago < 30 & days_ago >= 0 & days_ago %% 4 == 0
    # ) %>%
    
    left_join(
      occupancy_data %>%
        select(date, group, count) %>%
        pivot_wider(names_from = group, values_from = count),
      
      by = "date") %>%
    
    mutate(ward_vec = if_else(do_match, ward, -1),
           ICU_vec = if_else(do_match, ICU, -1),
           
           ward_vec = replace_na(ward_vec, -1),
           ICU_vec = replace_na(ICU_vec, -1))
  
  # Define our priors for length-of-stay and pr_hosp adjustment
  prior_sigma_los <- if_else(do_ABC, 0.6, 0)
  prior_sigma_hosp <- if_else(do_ABC, 0.1, 0)
  
  # Define our thresholds for the adaptive ABC fitting
  thresholds <- c(0.2, 0.3, 0.4, 0.5, 10)
  
  

date_seq <- seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, "days")

gengamma_delay_order <- c("ward_to_discharge", "ward_to_ICU", "ward_to_death", "ICU_to_discharge", "ICU_to_postICU", "ICU_to_death",
                    "postICU_to_discharge", "postICU_to_death")

gengamma_delay <- read_csv("~/source/los_rates/results/fit_2022_09_05_2/distribution_fits.csv",
                           show_col_types = FALSE) %>%

  mutate(coding = factor(coding, levels = gengamma_delay_order)) %>%


  complete(age, coding, date = date_seq) %>%
  arrange(age, coding, date) %>%
  group_by(age, coding) %>%

  mutate(across(c(mu, sigma, Q, p_trans), ~ zoo::na.approx(., na.rm = FALSE))) %>%

  fill(mu, sigma, Q, p_trans, .direction = "down") %>%
  ungroup() %>%

  filter(date >= date_seq[1], date <= date_seq[length(date_seq)]) %>%



  arrange(age, coding, date) %>%

  select(age, coding, date, mu, sigma, Q, p_trans) %>%

  mutate(age = age / 10,
         coding = match(coding, gengamma_delay_order) - 1,
         date = match(date, date_seq) - 1) %>%

  rename(pr_trans = p_trans)

  
  # Perform the actual simulation and fitting
  a <- Sys.time()
  results <- curvemush::mush_abc(
    n_samples = 4000,
    n_delay_samples = 512,
    
    n_outputs = 100,
    
    n_days = n_days,
    steps_per_day = 4,
    
    do_ABC = do_ABC,
    thresholds = thresholds,
    rejections_per_selections = if_else(do_ABC, 1000, 1),
    
    prior_sigma_los = prior_sigma_los,
    prior_sigma_hosp = prior_sigma_hosp,
    
    ensemble_curves = case_matrix,
    
    forecasting_parameters = clinical_parameter_samples,
    
    known_ward_vec = occupancy_curve_match$ward_vec,
    known_ICU_vec = occupancy_curve_match$ICU_vec,
    
    mat_pr_age_given_case = mat_pr_age_given_case,
    mat_pr_hosp = mat_pr_hosp,
    mat_pr_ICU = mat_pr_ICU#,
    
    
    #delay_parameters = gengamma_delay
    
  )
  
  b <- Sys.time()
  print(str_c("Simulation ran in ", round(b - a, 2), " ", units(b - a)))
  
  
  # Do a bunch of work to re-format the simulation output into a nicer form
  
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


make_results_quants <- function(tbl, probs = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
  data_matrix <- tbl %>%
    select(starts_with("sim_")) %>%
    as.matrix()
  
  id_tbl <- tbl %>%
    select(!starts_with("sim_"))
  
  medians <- data_matrix %>%
    matrixStats::rowMedians() %>%
    tibble(median = .)
  
  quant_probs <- c(rev(1 - probs) / 2, 0.5 + probs / 2)
  quant_names <- c(str_c("lower_", rev(probs) * 100), str_c("upper_", probs * 100))
  
  quants <- data_matrix %>%
    matrixStats::rowQuantiles(probs = quant_probs) %>%
    `colnames<-`(quant_names) %>%
    as_tibble() %>%
    bind_cols(id_tbl, .) %>%
    pivot_longer(cols = -all_of(colnames(id_tbl)),
                 names_to = c("type", "quant"),
                 names_sep = "_") %>%
    pivot_wider(names_from = "type",
                values_from = "value") %>%
    
    mutate(quant = factor(quant, levels = as.character(probs * 100)) %>% fct_rev())
  
  quants
}

