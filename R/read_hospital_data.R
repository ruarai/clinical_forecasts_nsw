






read_NSW_hospital_data <- function(hospital_data_path) {
  
  source("R/age_groups.R")
  
  linelist_raw <- readxl::read_xlsx(hospital_data_path, sheet = 2)
  
  read_NSW_linelist(linelist_raw, remove_adm_delay = FALSE, remove_sep_episodes = FALSE) %>%
    mutate(age_group = assign_10yr_age_group(age),
           date_data = as_date(linelist_raw$load_date[1]))
}



read_NSW_linelist <- function(
  linelist_raw,
  
  remove_adm_delay = TRUE,
  remove_sep_episodes = TRUE,
  
  return_diagnostics = FALSE
) {
  clinical_linelist <- linelist_raw %>%
    select(person_id, age, load_date,
           admit_date_dt, discharge_date_dt, first_icu_date_dt, last_icu_date_dt,
           still_in_hosp, still_in_icu, any_icu_flag, discharge_desc = AP_DISCHARGE_DISPOSITION_DESC,
           true_icu_hours = icu_hours,
           days_onset_to_adm = covid_to_adm,
           ward = WARD_TYPE,
           subward = SUB_WARD_TYPE)
  
  load_date <- first(clinical_linelist$load_date)
  
  time_diff_to_days <- function(time_diff){
    as.numeric(time_diff / ddays(1))
  }
  
  is_date_too_early <- function(dates) {
    replace_na(dates <= ymd("2020-06-01"), FALSE)
  }
  
  is_date_too_late <- function(dates) {
    replace_na(dates > load_date + days(2), FALSE)
  }
  
  is_indicator_incorrect <- function(discharge, still_in) {
    replace_na(!(still_in == 1 & discharge >= load_date - days(1) |
                   still_in == 0 & discharge <= load_date), FALSE)
  }
  
  is_duration_too_long <- function(start, end) {
    days <- time_diff_to_days(end - start)
    replace_na(days > 120, FALSE)
  }
  
  
  
  
  single_episodes <- clinical_linelist %>%
    group_by(person_id) %>%
    filter(n() == 1)
  
  multiple_episodes <- clinical_linelist %>%
    group_by(person_id) %>%
    filter(n() > 1) %>%
    arrange(person_id, admit_date_dt) %>%
    mutate(discharge_to_next_admit = time_diff_to_days(lead(admit_date_dt) - discharge_date_dt),
           discharge_to_next_admit = replace_na(discharge_to_next_admit, 0))
  
  
  if(remove_sep_episodes) {
    multiple_episodes_filt <- multiple_episodes %>%
      
      filter(all(discharge_to_next_admit < 2),
             all(discharge_to_next_admit > -1))
  } else{
    multiple_episodes_filt <- multiple_episodes %>%
      
      filter(all(discharge_to_next_admit < 5),
             all(discharge_to_next_admit > -1),
             n() < 5) %>%
      
      ungroup() %>%
      map_df(rev) %>%
      group_by(person_id) %>%
      
      mutate(days_to_add = ddays(cumsum(discharge_to_next_admit)),
             
             admit_date_dt = admit_date_dt + days_to_add,
             discharge_date_dt = discharge_date_dt + days_to_add,
             
             first_icu_date_dt = first_icu_date_dt + days_to_add,
             last_icu_date_dt = last_icu_date_dt + days_to_add) %>%
      
      ungroup() %>%
      map_df(rev) %>%
      group_by(person_id)
  }
  
  collapsed_multiple_episodes <- multiple_episodes_filt %>%
    
    mutate(discharge_date_dt = max(discharge_date_dt),
           
           first_icu_date_dt = suppressWarnings(min(first_icu_date_dt, na.rm = TRUE)),
           last_icu_date_dt = suppressWarnings(max(last_icu_date_dt, na.rm = TRUE)),
           true_icu_hours = suppressWarnings(sum(true_icu_hours, na.rm = TRUE)),
           
           any_icu_flag = suppressWarnings(any(any_icu_flag, na.rm = TRUE)),
           still_in_icu = suppressWarnings(any(still_in_icu, na.rm = TRUE)),
           
           still_in_hosp = if_else(any(still_in_hosp == 1), 1, 0),
           
           first_icu_date_dt = if_else(any_icu_flag, first_icu_date_dt, NA_POSIXct_),
           last_icu_date_dt = if_else(any_icu_flag, last_icu_date_dt, NA_POSIXct_)) %>%
    
    slice(1)
  
  
  
  clinical_linelist_collapsed <- bind_rows(
    collapsed_multiple_episodes,
    single_episodes
  ) %>%
    ungroup() %>%
    arrange(person_id)
  
  
  early_dated_entries <- clinical_linelist_collapsed %>%
    filter(if_any(ends_with("_date_dt"), ~ is_date_too_early(.)))
  
  late_dated_entries <- clinical_linelist_collapsed %>%
    filter(if_any(ends_with("_date_dt"), ~ is_date_too_late(.)))
  
  incorrect_indicator_dates <- clinical_linelist_collapsed %>%
    filter(is_indicator_incorrect(last_icu_date_dt, still_in_icu) |
             is_indicator_incorrect(discharge_date_dt, still_in_hosp))
  
  long_duration_entries <-  clinical_linelist_collapsed %>%
    filter(is_duration_too_long(first_icu_date_dt, last_icu_date_dt) |
             is_duration_too_long(admit_date_dt, discharge_date_dt))
  
  
  # A negative admission delay is a good indicator of a hospital stay
  # where covid was simply incidental
  unlikely_admission_delay <- clinical_linelist_collapsed %>%
    filter(days_onset_to_adm < 0)
  
  
  
  print("Rows with dates too early:")
  print(early_dated_entries)
  
  print("Rows with dates too late:")
  print(late_dated_entries)
  
  print("Rows with incorrect indicator dates:")
  print(incorrect_indicator_dates)
  
  print("Rows with excessive duration lengths:")
  print(long_duration_entries)
  
  print("Rows with unlikely admission delays:")
  print(unlikely_admission_delay)
  print(unlikely_admission_delay %>% pull(days_onset_to_adm))
  
  if(remove_adm_delay) {
    filtered_clinical_linelist <- setdiff(
      clinical_linelist_collapsed,
      early_dated_entries %>% 
        union(late_dated_entries) %>%
        union(incorrect_indicator_dates) %>%
        union(long_duration_entries) %>%
        union(unlikely_admission_delay)
    )
  } else{
    filtered_clinical_linelist <- setdiff(
      clinical_linelist_collapsed,
      early_dated_entries %>% 
        union(late_dated_entries) %>%
        union(incorrect_indicator_dates) %>%
        union(long_duration_entries)
    )
  }
  
  did_patient_die <- function(discharge_description) {
    death_descriptors <- c("deceased", "death", "died", "dead")
    regex_match <- str_c("(?i)(", str_c(death_descriptors, collapse = "|"), ")")
    
    str_detect(discharge_description, regex_match)
  }
  
  cleaned_clinical_linelist <- filtered_clinical_linelist %>%
    mutate(is_still_in_hosp = still_in_hosp == 1,
           is_still_in_icu = still_in_icu == 1,
           ever_in_icu = any_icu_flag == 1,
           patient_died = did_patient_die(discharge_desc),
           
           date_onset = as_date(admit_date_dt - ddays(days_onset_to_adm))) %>%
    
    
    select(case_id = person_id,
           age,
           dt_hosp_admission = admit_date_dt,
           dt_hosp_discharge = discharge_date_dt,
           dt_first_icu = first_icu_date_dt,
           dt_last_icu = last_icu_date_dt,
           date_onset,
           true_icu_hours,
           
           is_still_in_hosp,
           is_still_in_icu,
           ever_in_icu,
           patient_died)
  
  
  if(return_diagnostics) {
    filtered_multi_episodes <- multiple_episodes %>%
      
      filter(any(discharge_to_next_admit >= 2)|
               any(discharge_to_next_admit <= -1))
    
    n_ind <- . %>% pull(person_id) %>% unique() %>% length()
    
    adm_delay_inds <- unique(unlikely_admission_delay$person_id)
    
    bad_duration_inds <- union(
      union(unique(late_dated_entries$person_id), unique(incorrect_indicator_dates$person_id)),
      union(unique(early_dated_entries$person_id), unique(long_duration_entries$person_id))
    )%>%
      setdiff(adm_delay_inds)
    
    
    
    return(list(
      data = cleaned_clinical_linelist,
      
      diagnostics = tibble(
        n_raw_records = nrow(linelist_raw),
        n_raw_ind = linelist_raw  %>% n_ind(),
        
        n_single_episode_ind = nrow(single_episodes),
        n_mult_episode_ind = multiple_episodes %>% n_ind(),
        
        n_invalid_multi_episode_ind = filtered_multi_episodes %>% n_ind(),
        n_after_multi_episode_filt_ind = linelist_raw  %>% n_ind() - filtered_multi_episodes %>% n_ind(),
        
        n_adm_delay_ind = length(adm_delay_inds),
        n_bad_duration_ind = length(bad_duration_inds),
        
        final_n_ind = filtered_clinical_linelist %>% n_ind()
      )
    ))
  }
  else{
    return(cleaned_clinical_linelist)
  }
}
