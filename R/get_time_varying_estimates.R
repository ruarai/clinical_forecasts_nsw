

get_time_varying_estimates <- function(
  nsw_cases,
  hospital_data_unfiltered,
  clinical_parameters,
  forecast_dates
) {
  source("R/age_groups.R")
  
  
  # When to assume data may be affected by right truncation (~3 days prior to earliest load date)
  date_cutoff <- min(forecast_dates$case_data, forecast_dates$hospital_data) - ddays(3)
  
  # Clean up and count the NSW data
  nsw_cases_count <- nsw_cases %>%
    mutate(date_onset = EARLIEST_CONFIRMED_OR_PROBABLE - ddays(2),
           age_group = assign_10yr_age_group(AGE_AT_EVENT_YEARS)) %>%
    filter(date_onset >= forecast_dates$simulation_start,
           date_onset < date_cutoff) %>%
    count(date_onset, age_group)
  
  # Clean up and count the hospital data also
  hospital_admissions_count <- hospital_data_unfiltered %>%
    
    count(date_onset, age_group, ever_in_ICU = ever_in_icu) %>%
    filter(date_onset >= forecast_dates$simulation_start,
           date_onset < date_cutoff) %>%
    pivot_wider(names_from = ever_in_ICU, values_from = n) %>%
    `colnames<-`(c("date_onset", "age_group", "hospitalised", "hospitalised_and_ICU")) %>%
    mutate(hospitalised_and_ICU = replace_na(hospitalised_and_ICU, 0),
           hospitalised = replace_na(hospitalised, 0),
           hospitalised_count = hospitalised + hospitalised_and_ICU) %>%
    select(-hospitalised) %>%
    
    rename(ICU_count = hospitalised_and_ICU)
  
  
  # Join the two counts
  joint_count <- hospital_admissions_count %>%
    full_join(nsw_cases_count %>% rename(case_count = n),
              by = c("date_onset", "age_group")) %>%
    
    mutate(hospitalised_count = replace_na(hospitalised_count, 0),
           ICU_count = replace_na(ICU_count, 0),
           case_count = if_else(is.na(case_count), hospitalised_count, case_count),
           case_count = if_else(hospitalised_count > case_count, hospitalised_count, case_count)) 
  
  # Expand the counts out into a linelist (again) so that we can do bootstrapping
  cases_expanded <- pmap_dfr(
    joint_count,
    function(date_onset, age_group, case_count, hospitalised_count, ICU_count) {
      bind_rows(
        tibble(
          date_onset = date_onset,
          age_group = age_group,
          ever_in_hospital = TRUE,
          ever_in_ICU = TRUE
        ) %>% slice(rep(1, times = ICU_count)),
        tibble(
          date_onset = date_onset,
          age_group = age_group,
          ever_in_hospital = TRUE,
          ever_in_ICU = FALSE
        ) %>% slice(rep(1, times = hospitalised_count - ICU_count)),
        tibble(
          date_onset = date_onset,
          age_group = age_group,
          ever_in_hospital = FALSE
        ) %>% slice(rep(1, times = case_count - hospitalised_count))
        
        
      )
    }
  )
  

  # When are our estimations being performed?
  estimation_period <- c(forecast_dates$simulation_start, date_cutoff)
  estimation_period_days <- seq(estimation_period[1], estimation_period[2], by = 'days')
  
  # How many bootstraps to perform for each estimate?
  n_bootstraps <- 50
  
  # How wide is our running average? 1 week seems to work okay with current case numbers
  window_width <- 7
  
  # Define the vector of difference estimation windows
  window_starts <- 1:(length(estimation_period_days) - window_width)
  window_ends <- window_starts + window_width
  
  source("R/age_groups.R")
  
  # Produce n_bootstraps bootstraps of the case data
  age_tables <- map(
    1:n_bootstraps,
    function(i_bootstrap) {
      cases_expanded %>%
        sample_n(n(), replace = TRUE) %>%
        count(date_onset, age_group) %>%
        complete(
          date_onset = estimation_period_days,
          age_group = age_groups,
          
          fill = list(n = 0)
        ) %>%
        group_by(date_onset) %>%
        mutate(n_all_ages = sum(n)) %>%
        ungroup() %>%
        arrange(age_group) %>%
        
        mutate(days_since_onset = as.numeric(forecast_dates$hospital_data - date_onset),
               .before = 1) %>%
        select(-date_onset) %>%
        
        arrange(desc(days_since_onset))
    }
  )
  
  # Produce simple estimates of the multinomial distribution of case ages
  age_results <- map(
    age_tables,
    function(i_bootstraped_age_table) {
      map(
        age_groups,
        function(i_age_group) {
          i_age_table <- i_bootstraped_age_table %>%
            filter(age_group == i_age_group)
          
          map_dbl(
            1:length(window_starts),
            function(i_window) {
              window_start <- window_starts[i_window]
              window_end <- window_ends[i_window]
              
              n_cases_age_group <- i_age_table$n[window_start:window_end]
              n_cases_all_ages <- i_age_table$n_all_ages[window_start:window_end]
              
              if(sum(n_cases_all_ages) == 0)
                return(0)
              
              return(sum(n_cases_age_group) / sum(n_cases_all_ages))
            }
          )
          
          
        }
      )
    }
  )
  
  # As above, produce bootstraps of the number of individuals hospitalised
  hosp_tables <- map(
    1:n_bootstraps,
    function(i_bootstrap) {
      cases_expanded %>%
        sample_n(n(), replace = TRUE) %>%
        count(date_onset, age_group, ever_in_hospital) %>%
        complete(
          date_onset = estimation_period_days,
          age_group = age_groups,
          ever_in_hospital = c(TRUE, FALSE),
          
          fill = list(n = 0)
        ) %>%
        
        arrange(age_group, date_onset, ever_in_hospital) %>%
        mutate(ever_in_hospital = if_else(ever_in_hospital, "n_hosp", "n_not_hosp")) %>%
        
        pivot_wider(names_from = ever_in_hospital,
                    values_from = n) %>%
        
        mutate(n_cases = n_not_hosp + n_hosp,
               .before = "n_not_hosp") %>%
        
        mutate(days_since_onset = as.numeric(forecast_dates$hospital_data - date_onset),
               .before = 1) %>%
        select(-date_onset) %>%
        arrange(desc(days_since_onset))
    }
  ) 
  
  
  
  
  fn_score_hosp <- function(x, n_hosp, not_hosp_days_since_onset, n_not_hosp, delay_shape, delay_scale) {
    prob_already_observed <- pgamma(
      not_hosp_days_since_onset,
      shape = delay_shape,
      scale = delay_scale
    )
    
    
    sum(n_hosp) / x - sum(n_not_hosp * (prob_already_observed / (1 - x * prob_already_observed)))
  }
  
  clinical_parameter_lookup <- clinical_parameters %>%
    
    select(-age_group) %>%
    as.matrix() %>%
    `rownames<-`(clinical_parameters$age_group)
  
  
  # MLE estimation of right-truncation adjusted probability of hospiutalisation across the bootstrapped datasets
  hosp_results <- map(
    hosp_tables,
    function(i_bootstrapped_hosp_table) {
      map(
        age_groups,
        function(i_age_group) {
          i_hosp_table <- i_bootstrapped_hosp_table %>%
            filter(age_group == i_age_group)
          
          
          delay_hosp_shape <- clinical_parameter_lookup[i_age_group, "shape_onset_to_ward"]
          delay_hosp_scale <- clinical_parameter_lookup[i_age_group, "scale_onset_to_ward"]
          
          map_dbl(
            1:length(window_starts),
            function(i_window) {
              window_start <- window_starts[i_window]
              window_end <- window_ends[i_window]
              
              days_since_onset <- i_hosp_table$days_since_onset[window_start:window_end]
              n_hosp <- i_hosp_table$n_hosp[window_start:window_end]
              n_not_hosp <- i_hosp_table$n_not_hosp[window_start:window_end]
              
              if(sum(n_hosp) == 0)
                return(0)
              if(sum(n_not_hosp) == 0)
                return(1)
              
              if(max(days_since_onset) > 45)
                return(sum(n_hosp) / sum(n_hosp + n_not_hosp))
              
              tryCatch(pracma::fzero(
                function(x) {
                  fn_score_hosp(x, n_hosp, days_since_onset, n_not_hosp, delay_shape = delay_hosp_shape, delay_scale = delay_hosp_scale)
                },
                
                x = c(0 + .Machine$double.eps, 1 - .Machine$double.eps),
                tol = 0.0001
              )$x, error = function(e) return(NA))
            }
          )
          
          
        }
      )
    }
  )
  
  
  # Repeating the above for ICU probabilities
  
  # Make a lookup table for total ICU delays to speed this up
  F_ICU_lookup <- get_ICU_lookup(age_groups, clinical_parameter_lookup)
  
  F_icu_given_case <- function(x, age_group) {
    F_ICU_lookup[min(round(x, digits = 2) * 10 + 1, 301), age_group]
  }
  
  fn_score_ICU <- function(x, n_ICU, n_not_ICU, not_ICU_days_since_onset, age_group) {
    
    prob_already_observed <- sapply(1:length(not_ICU_days_since_onset), function(i) {
      F_icu_given_case(not_ICU_days_since_onset[i], age_group)
    })
    
    sum(n_ICU) / x - sum(n_not_ICU * (prob_already_observed / (1 - x * prob_already_observed)))
  }
  
  
  cases_hospitalised <- cases_expanded %>%
    filter(ever_in_hospital)
  
  ICU_tables <- map(
    1:n_bootstraps,
    function(i_bootstrap) {
      cases_hospitalised %>%
        sample_n(n(), replace = TRUE) %>%
        count(date_onset, age_group, ever_in_ICU) %>%
        complete(
          date_onset = estimation_period_days,
          age_group = age_groups,
          ever_in_ICU = c(TRUE, FALSE),
          
          fill = list(n = 0)
        ) %>%
        
        arrange(age_group, date_onset, ever_in_ICU) %>%
        mutate(ever_in_ICU = if_else(ever_in_ICU, "n_ICU", "n_not_ICU")) %>%
        
        pivot_wider(names_from = ever_in_ICU,
                    values_from = n) %>%
        
        mutate(n_hosp = n_not_ICU + n_ICU,
               .before = "n_not_ICU") %>%
        
        mutate(days_since_onset = as.numeric(forecast_dates$hospital_data - date_onset),
               .before = 1) %>%
        select(-date_onset) %>%
        arrange(desc(days_since_onset))
    }
  ) 
  
  ICU_results <- map(
    ICU_tables,
    function(i_bootstrapped_ICU_table) {
      map(
        age_groups,
        function(i_age_group) {
          i_ICU_table <- i_bootstrapped_ICU_table %>%
            filter(age_group == i_age_group)
          
          map_dbl(
            1:length(window_starts),
            function(i_window) {
              window_start <- window_starts[i_window]
              window_end <- window_ends[i_window]
              
              days_since_onset <- i_ICU_table$days_since_onset[window_start:window_end]
              n_ICU <- i_ICU_table$n_ICU[window_start:window_end]
              n_not_ICU <- i_ICU_table$n_not_ICU[window_start:window_end]
              
              if(sum(n_ICU) == 0)
                return(0)
              if(sum(n_not_ICU) == 0)
                return(1)
              
              if(max(days_since_onset) > 30)
                return(sum(n_ICU) / sum(n_ICU + n_not_ICU))
              
              tryCatch(pracma::fzero(
                function(x) {
                  fn_score_ICU(
                    x,
                    n_ICU,
                    n_not_ICU,
                    days_since_onset,
                    i_age_group
                  )
                },
                
                x = c(0+.Machine$double.eps,1-.Machine$double.eps),
              )$x, error = function(e) return(NA))
            }
          )
          
          
        }
      )
    }
  )
  
  
  # Join everything together into a dataframe
  # This is messy but faster than map_dfr
  
  all_results_ls <- vector(mode = "list", length = n_bootstraps * length(age_groups))
  
  
  j <- 1
  for(i_bootstrap in 1:n_bootstraps) {
    for(i_age_group in 1:length(age_groups)) {
      all_results_ls[[j]] <- tibble(
        bootstrap = i_bootstrap,
        age_group = age_groups[i_age_group],
        window_start = window_starts,
        
        pr_age_given_case = age_results[[i_bootstrap]][[i_age_group]],
        pr_hosp = hosp_results[[i_bootstrap]][[i_age_group]],
        pr_ICU = ICU_results[[i_bootstrap]][[i_age_group]]
      )
      
      j <- j + 1
      
      
    }
  }
  
  all_results <- bind_rows(all_results_ls) %>%
    mutate(window_start = window_start - 1 + estimation_period[1]) %>%
    mutate(date = window_start + window_width / 2) %>%
    select(-window_start)
  
  
  # Cleanup and return
  all_results %>%
    mutate(date = floor_date(date, "days")) %>%
    
    complete(
      bootstrap = 1:n_bootstraps,
      age_group = age_groups,
      date = floor_date(seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, by = "days"), "days")
    ) %>%
    
    arrange(date) %>%
    
    group_by(bootstrap, age_group) %>%
    
    fill(pr_age_given_case, pr_hosp, pr_ICU, .direction = "updown") %>%
    ungroup()
}





get_ICU_lookup <- function(age_groups, clinical_parameter_lookup) {
  
  tbl_vars <- expand_grid(
    x = seq(0, 30, by = 0.1),
    age_group = age_groups
  )
  
  
  F_icu_given_case_exact <- function(x, 
                                     delay_hosp_shape, delay_hosp_scale,
                                     delay_ICU_shape, delay_ICU_scale) {
    
    numer <- integrate(function(y){
      pgamma(x - y,
             shape = delay_ICU_shape,
             scale = delay_ICU_scale) *
        dgamma(y,
               shape = delay_hosp_shape,
               scale = delay_hosp_scale)
    },
    
    0 + .Machine$double.eps, x)$value
    
    denom <- pgamma(x,
                    shape = delay_hosp_shape,
                    scale = delay_hosp_scale)
    
    return(numer / denom)
  }
  
  y_vals <- pmap_dbl(tbl_vars, function(x, age_group) {
    
    
    F_icu_given_case_exact(
      x, 
      delay_hosp_shape = clinical_parameter_lookup[age_group, "shape_onset_to_ward"],
      delay_hosp_scale = clinical_parameter_lookup[age_group, "scale_onset_to_ward"],
      
      delay_ICU_shape = clinical_parameter_lookup[age_group,  "shape_ward_to_ICU"],
      delay_ICU_scale = clinical_parameter_lookup[age_group, "scale_ward_to_ICU"]
    )
  })
  
  
  tbl_output <- tbl_vars %>%
    mutate(y = y_vals,
           
           y = if_else(is.nan(y), 0, y))
  
  
  tbl_output %>%
    pivot_wider(names_from = age_group,
                values_from = y) %>%
    select(-x) %>%
    as.matrix()
  
}











