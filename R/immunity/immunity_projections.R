


get_immune_predictions <- function(
  prediction_dates,
  nindss_state,
  neuts_change,
  case_trajectories,
  population_state,
  vaccination_tables_state,
  vaccination_effects_state,
  forecast_dates
) {
  ngm <- read_rds("data/vaccination/ngm_quantium.rds")
  
  n_preds <- length(prediction_dates)
  
  # Sum the case_trajectories into 4-day (1 generation) width buckets
  summed_cases <- matrix(0, nrow = n_preds, ncol = ncol(case_trajectories$curve_set))
  for(i in 0:3) {
    ix_sum <- seq(1, case_trajectories$n_days, by = 4) + i
    ix_sum <- pmin(ix_sum, case_trajectories$n_days)
    summed_cases <- summed_cases + case_trajectories$curve_set[ix_sum, ]
  }
  
  
  vaccination_acquisition_effect <- do.call(rbind, vaccination_effects_state$acquisition_effect)
  vaccination_transmission_effect <- do.call(rbind, vaccination_effects_state$transmission_effect)
  vaccination_hospitalisation_effect <- do.call(rbind, vaccination_effects_state$hospitalisation_effect)
  vaccination_coverage <- do.call(rbind, vaccination_effects_state$prop_coverage)
  
  
  
  case_counts_mat <- nindss_state %>%
    select(age, date_onset) %>%
    
    drop_na(age) %>%
    mutate(age_band = assign_age_band(age)) %>%
    
    count(date_onset, age_band) %>%
    filter(date_onset >= prediction_dates[1]) %>%
    rowwise() %>%
    mutate(date_nearest = prediction_dates[which.min(abs(prediction_dates - date_onset))]) %>%
    
    group_by(date = date_nearest, age_band) %>%
    summarise(n = sum(n), .groups = "drop") %>% 
    
    complete(date = prediction_dates,
             age_band = age_band_order,
             fill = list(n = 0)) %>%
    
    filter(date <= forecast_dates$NNDSS)  %>%
    
    mutate(age_band = factor(age_band, levels = age_band_order)) %>%
    arrange(age_band) %>%
    
    pivot_wider(names_from = age_band,
                values_from = n) %>%
    
    select(-date) %>%
    as.matrix()
  
  vec_population <- population_state$population
  
  get_neuts_effect_infection <- function(mean_neuts, c50_vec) {
    reff_env$ve_from_mean_log10_neut(
      mean_log10_neut_vec = mean_neuts,
      sd_log10_neut = params$ve$sd_log10_neut_titres,
      log_k = params$ve$log_k,
      c50_vec = c50_vec,
      method = "gaussian"
    )
  }
  neuts_range <- seq(-5, 5, 0.01)
  ve_acquisition_lookup <- get_neuts_effect_infection(neuts_range, params$ve$c50_acquisition)
  ve_transmission_lookup <- get_neuts_effect_infection(neuts_range, params$ve$c50_transmission)
  ve_hospitalisation_lookup <- get_neuts_effect_infection(neuts_range, params$ve$c50_hospitalisation)
  
  lookup_tables <- list("acquisition" = ve_acquisition_lookup,
                        "transmission" = ve_transmission_lookup,
                        "hospitalisation" = ve_hospitalisation_lookup)
  
  get_immune_efficacy <- function(x_neuts, effect) {
    lookup_ix <- (round(x_neuts, digits = 2) + 5) * 100 + 1
    
    clean_ix <- if_else(lookup_ix < 0 | lookup_ix > length(neuts_range), 1, lookup_ix)
    
    if_else(lookup_ix < 0 | lookup_ix > length(neuts_range),
            rep(0, length(lookup_ix)),
            lookup_tables[[effect]][clean_ix])
  }
  
  log10_neut_over_time_infection <- reff_env$log10_neut_over_time(
    seq(1, 1000, by = 4),
    params$neut$log10_mean_neut_infection,
    params$neut$neut_decay
  )
  
  
  ascertainment_ts <- get_ascertainment_timeseries(prediction_dates)$ascertainment
  ascertainment_mat <- matrix(ascertainment_ts, ncol = 17, nrow = length(ascertainment_ts))
  
  n_sims <- ncol(summed_cases)
  
  
  vaccination_acquisition_effect <- do.call(rbind, vaccination_effects_state$acquisition_effect)
  vaccination_transmission_effect <- do.call(rbind, vaccination_effects_state$transmission_effect)
  vaccination_hospitalisation_effect <- do.call(rbind, vaccination_effects_state$hospitalisation_effect)
  vaccination_coverage <- do.call(rbind, vaccination_effects_state$prop_coverage)
  
  print("Starting iterative projection...")
  
  library(future.callr)
  plan(callr)
  
  a <- Sys.time()
  results <- furrr::future_map(
    1:n_sims,
    simulate_immune_prediction,
    
    # Pass everything but the bigger objects (e.g. case data)
    .options = furrr::furrr_options(
      globals = c("simulate_immune_prediction",
                  "ascertainment_mat",
                  "log10_neut_over_time_infection",
                  "get_immune_efficacy", "lookup_tables", "neuts_change", "neuts_range",
                  "vec_population",
                  "vaccination_acquisition_effect", "vaccination_transmission_effect",
                  "vaccination_hospitalisation_effect", "vaccination_coverage",
                  "vaccination_tables_state",
                  "summed_cases", "case_counts_mat", "ngm", "n_preds",
                  "prediction_dates")
    )
  )
  print(Sys.time() - a)
  
  
  bind_rows(results)
}



simulate_immune_prediction <- function(i_sim) {
  
  suppressPackageStartupMessages(library(tidyverse))
  source("R/immunity/age_data.R")
  
  pred_case_counts_mat <- case_counts_mat
  pred_acquisition_multiplier <- matrix(nrow = n_preds, ncol = 17)
  pred_transmission_multiplier <- matrix(nrow = n_preds, ncol = 17)
  pred_hospitalisation_multiplier <- matrix(nrow = n_preds, ncol = 17)
  
  for(pred_ix in 1:n_preds) {
    
    vaccination_neut_offset <- neuts_change$vaccine_neuts_drop[[pred_ix]]
    
    infection_neut_offset <- neuts_change$case_neuts_drop[1:pred_ix] * neuts_change$proportionBA4BA5[[pred_ix]]
    
    previous_infections <- t(apply(
      pred_case_counts_mat[1:pred_ix, ,drop = FALSE] / ascertainment_mat[1:pred_ix, ],
      1, function(x) x / vec_population
    ))
    
    # Limit past distribution to ignore infections prior to what is cumulatively 100% (e.g. excluding original infections when including reinfections)
    previous_infections <- matrix(apply(previous_infections, 2, function(x) if_else(rev(cumsum(rev(x))) > 1, 0, x)), ncol = 17)
    
    infection_coverage <- colSums(previous_infections)
    infection_coverage <- pmin(infection_coverage, 1)
    
    mean_neuts_infection <- colSums((infection_neut_offset + log10_neut_over_time_infection[pred_ix:1]) * previous_infections) / 
      infection_coverage
    
    mean_neuts_infection <- replace_na(mean_neuts_infection, -100)
    
    infection_acquisition_effect <- get_immune_efficacy(mean_neuts_infection, "acquisition")
    infection_transmission_effect <- get_immune_efficacy(mean_neuts_infection, "transmission")
    infection_hospitalisation_effect <- get_immune_efficacy(mean_neuts_infection, "hospitalisation")
    
    pretable_mat <- vaccination_tables_state[[pred_ix]] %>%
      select(-days_i) %>%
      as.matrix()
    
    
    
    mean_neuts_infection_vacc <- colSums((infection_neut_offset + pretable_mat) * previous_infections) / 
      infection_coverage
    
    mean_neuts_infection_vacc <- replace_na(mean_neuts_infection_vacc, -100)
    
    infection_vacc_acquisition_effect <- get_immune_efficacy(mean_neuts_infection_vacc, "acquisition")
    infection_vacc_transmission_effect <- get_immune_efficacy(mean_neuts_infection_vacc, "transmission")
    infection_vacc_hospitalisation_effect <- get_immune_efficacy(mean_neuts_infection_vacc, "hospitalisation")
    
    
    
    p_infected_only <- infection_coverage * (1 - vaccination_coverage[pred_ix,])
    p_vaccinated_only <- vaccination_coverage[pred_ix, ] * (1 - infection_coverage)
    
    
    p_infandvax <- infection_coverage * vaccination_coverage[pred_ix, ]
    
    weighted_acquisition <- p_infected_only * infection_acquisition_effect + 
      p_vaccinated_only * vaccination_acquisition_effect[pred_ix, ] +
      p_infandvax * infection_vacc_acquisition_effect
    
    weighted_transmission <- p_infected_only * infection_transmission_effect + 
      p_vaccinated_only * vaccination_transmission_effect[pred_ix, ] + 
      p_infandvax * infection_vacc_transmission_effect
    
    
    p_none <- 1 - (p_infandvax + p_vaccinated_only + p_infected_only)
    
    p_infected_only_is_infected <- p_infected_only * (1 - infection_acquisition_effect)
    p_vaccinated_only_is_infected <- p_vaccinated_only * (1 - vaccination_acquisition_effect[pred_ix,])
    p_infandvax_is_infected <- p_infandvax * (1 - infection_vacc_acquisition_effect) 
    adj_amount <- 1 / (p_infected_only_is_infected + p_vaccinated_only_is_infected + p_infandvax_is_infected + p_none)
    
    weighted_hospitalisation <- (p_infected_only_is_infected * adj_amount) * infection_hospitalisation_effect +
      (p_vaccinated_only_is_infected * adj_amount) * vaccination_hospitalisation_effect[pred_ix, ] +
      (p_infandvax_is_infected * adj_amount) * infection_vacc_hospitalisation_effect
    
    
    acquisition_multiplier <- 1 - weighted_acquisition
    transmission_multiplier <- 1 - weighted_transmission
    hospitalisation_multiplier <- 1 - weighted_hospitalisation
    
    pred_acquisition_multiplier[pred_ix, ] <- acquisition_multiplier
    pred_transmission_multiplier[pred_ix, ] <- transmission_multiplier
    pred_hospitalisation_multiplier[pred_ix, ] <- hospitalisation_multiplier
    
    if(pred_ix + 1 > nrow(pred_case_counts_mat) & pred_ix != n_preds) {
      ngm_mult <- acquisition_multiplier %*% t(transmission_multiplier)
      
      # Add 0.1 to avoid divide by zero
      prev_age_dist <- (previous_infections[pred_ix, ] + 0.1) / sum(previous_infections[pred_ix, ] + 0.1)
      
      
      pred_case_age_dist <- ((ngm * ngm_mult) %*% prev_age_dist) * ascertainment_mat[pred_ix, ]
      
      prop_pred_case_age_dist <- pred_case_age_dist / sum(pred_case_age_dist)
      
      
      
      n_cases_total <- summed_cases[pred_ix, i_sim]
      
      new_cases <- matrix(t(prop_pred_case_age_dist) * n_cases_total, byrow = TRUE, ncol = 17, nrow = 1)
      
      if(any(is.na(new_cases))) {
        print(i_sim)
        stop("NA values in new_cases")
      }
      
      
      pred_case_counts_mat <- rbind(pred_case_counts_mat, new_cases)
    }
  }
  
  
  matrix_to_tbl <- function(mat, value_name) {
    mat %>%
      `colnames<-`(str_c("age_", age_band_order)) %>%
      as_tibble() %>%
      mutate(date = prediction_dates, .before = 1) %>%
      pivot_longer(-date, names_prefix = "age_", names_to = "age_band", values_to = value_name)
  }
  
  protections_tbl <- matrix_to_tbl(pred_acquisition_multiplier, "mult_acquisition") %>%
    left_join(matrix_to_tbl(pred_transmission_multiplier, "mult_transmission"), by = c("date", "age_band")) %>%
    left_join(matrix_to_tbl(pred_hospitalisation_multiplier, "mult_hospitalisation"), by = c("date", "age_band")) %>%
    
    pivot_longer(starts_with("mult_"),
                 names_to = "type", values_to = "multiplier") %>%
    
    mutate(age_band = factor(age_band, levels = age_band_order),
           sim = i_sim)
  
  age_dist_tbl <- (pred_case_counts_mat / rowSums(pred_case_counts_mat)) %>%
    `colnames<-`(str_c("age_", age_band_order)) %>%
    as_tibble() %>%
    mutate(date = prediction_dates, .before = 1) %>%
    pivot_longer(-date, names_prefix = "age_", names_to = "age_band", values_to = "pr_age_given_case") %>%
    
    mutate(age_band = factor(age_band, levels = age_band_order),
           sim = i_sim) %>%
    
    group_by(age_band) %>%
    arrange(date) %>%
    mutate(pr_age_given_case = zoo::rollmean(pr_age_given_case, 4, fill = NA),) %>%
    fill(pr_age_given_case, .direction = "updown") %>%
    ungroup()
  
  
  
  protections_tbl_grped <- protections_tbl %>%
    filter(type == "mult_hospitalisation") %>%
    select(-type) %>% 
    rename(m_hosp = multiplier)  %>%
    
    mutate(age_group = age_bands_to_groups(age_band)) %>%
    group_by(sim, date, age_group) %>%
    summarise(m_hosp = mean(m_hosp), .groups = "drop")
  
  
  age_dist_tbl_grped <- age_dist_tbl %>%
    mutate(age_group = age_bands_to_groups(age_band)) %>%
    group_by(sim, date, age_group) %>%
    summarise(pr_age_given_case = sum(pr_age_given_case), .groups = "drop")
  
  protections_tbl_grped %>%
    left_join(
      age_dist_tbl_grped, 
      by = c("sim", "date", "age_group")
    ) %>%
    
    group_by(sim, age_group) %>%
    arrange(date) %>%
    fill(pr_age_given_case, .direction = "updown") %>%
    
    ungroup() 
}
