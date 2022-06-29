

process_vaccination_data <- function(vaccination_data_state, prediction_dates) {
  tibble(
    date = prediction_dates
  ) %>%
    mutate(
      cohorts = map(
        .x = date,
        .f = reff_env$get_vaccine_cohorts_at_date,
        vaccine_scenarios = vaccination_data_state
      ),
      coverage = map(
        .x = cohorts,
        .f = reff_env$get_coverage
      )
    )
}

get_vaccination_effects <- function(vaccination_coverage_state, neuts_change) {
  get_ves <- function(date, cohorts, coverage) {
    neut_offset <- neuts_change$neuts_drop_absolute[[1]]
    ba45_prop <- neuts_change %>% 
      filter(date == !!date) %>%
      pull(proportionBA4BA5)
    
    reff_env$get_vaccine_efficacies(cohorts, 
                                    variants = c("Delta", "Omicron BA2", "Omicron BA4/5"),
                                    10^neut_offset) %>%
      filter(variant %in% c("Omicron BA2", "Omicron BA4/5"), omicron_scenario == "estimate",
             outcome %in% c("acquisition", "transmission", "hospitalisation")) %>%
      complete(age_band = age_band_order,
               scenario, state, omicron_scenario, variant, outcome,
               fill = list(ve = 0)) %>%
      pivot_wider(names_from = "outcome", values_from = "ve") %>%
      
      left_join(coverage, by = c("age_band", "scenario", "state")) %>%
      
      group_by(scenario, state, omicron_scenario, age_band) %>%
      mutate(proportion = if_else(variant == "Omicron BA4/5", ba45_prop, 1 - ba45_prop)) %>%
      summarise(across(c(acquisition, hospitalisation, transmission, coverage), ~ sum(proportion * .)), .groups = "drop") %>%
      
      mutate(age_band = factor(age_band, levels = age_band_order)) %>%
      arrange(age_band) %>%
      select(age_band, acquisition, transmission, hospitalisation, coverage)
  }
  
  
  vaccination_coverage_state %>%
    mutate(
      ves = pmap(
        .l = .,
        .f = get_ves
      ),
      acquisition_effect = map(.x = ves, .f = ~ .$acquisition),
      transmission_effect = map(.x = ves, .f = ~ .$transmission),
      hospitalisation_effect = map(.x = ves, .f = ~ .$hospitalisation),
      prop_coverage = map(.x = ves, .f = ~ .$coverage)
    ) %>%
    select(-cohorts, coverage)
}

get_state_population <- function(vaccination_data_state) {
  vaccination_data_state %>%
    filter(scenario == max(scenario)) %>%
    group_by(age_band, state) %>%
    summarise(
      population = sum(num_people, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    
    arrange(age_band)
}