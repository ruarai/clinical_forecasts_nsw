
preprepare_vaccination_tables <- function(vaccination_coverage_state, date_start) {
  library(future.callr)
  plan(callr)
  
  vaccination_coverage_state %>%
    
    furrr::future_pmap(
      prepare_table,
      
      .options = furrr::furrr_options(
        globals = c("prepare_table", "date_start", "params", "reff_env",
                    "age_band_order"),
        packages = c("dplyr", "tidyr")
      )
    )
}


prepare_table <- function(date, cohorts, ...) {
  #pb$tick()
  
  # Sequence that our infection data will align with
  inf_days_ago <- seq(0, as.numeric(date - date_start), by = 4)
  
  cohorts_tmp <- cohorts %>%
    # Remove the un-vaccinated
    filter(!is.na(days_ago)) %>%
    rename(
      num_people_v = num_people,
      days_v = days_ago
    ) %>%
    group_by(
      age_band
    ) %>%
    mutate(
      weight_v = num_people_v / sum(num_people_v)
    )  %>%
    mutate(omicron_scenario = "estimate") %>%
    left_join(
      params$neut,
      by = "omicron_scenario"
    ) %>%
    # compute the peak and waned log10 mean neuts for each cohort
    # where each cohort is that dose plus Omicron infection
    mutate(
      peak_neuts = case_when(
        immunity == "AZ_dose_1" ~ log10_mean_neut_AZ_dose_2,
        immunity == "AZ_dose_2" ~ log10_mean_neut_mRNA_booster,
        immunity == "Pf_dose_1" ~ log10_mean_neut_Pfizer_dose_2,
        immunity == "Pf_dose_2" ~ log10_mean_neut_mRNA_booster,
        immunity == "mRNA_dose_3" ~ log10_mean_neut_mRNA_booster,
        immunity == "mRNA_dose_4" ~ log10_mean_neut_mRNA_booster
      )
    )
  
  # Join with the infection-days-ago sequence (creating a full product)
  
  
  ## Do this iteratively to reduce memory requirement
  map_dfr(
    inf_days_ago,
    function(i) {
      cohorts_tmp %>%
        # days_ago will be the minimum of days since infection and days since vaccination
        mutate(days_ago = pmin(i, days_v)) %>%
        ungroup() %>%
        mutate(
          neuts = reff_env$log10_neut_over_time(
            time = days_ago,
            maximum_log10_neut = peak_neuts,
            decay = neut_decay
          )
        ) %>%
        select(
          -starts_with("log10_mean_neut"),
          -peak_neuts,
          -neut_decay
        ) %>%
        mutate(
          weighted_neuts = weight_v * neuts
        ) %>%
        # average the mean neuts over cohorts and scenarios for this inf_days_ago i
        group_by(
          omicron_scenario, age_band
        ) %>%
        summarise(
          neuts = sum(weighted_neuts),
          .groups = "drop"
        ) %>%
        mutate(days_i = i)
    }
  ) %>% 
    
    # Reshape into a table that will be easier to use later
    
    select(days_i, age_band, neuts) %>%
    complete(age_band = age_band_order, days_i, fill = list(neuts = -100))  %>% 
    mutate(age_band = factor(age_band, levels = age_band_order)) %>%
    arrange(age_band, desc(days_i)) %>%
    
    pivot_wider(names_from = age_band,
                values_from = neuts)
  
  
}


