


calculate_occupancy <- function(hospital_data) {
  
  full_occ <- left_join(
    hospital_data %>%
      mutate(date_start = pmax(as_date(dt_hosp_admission), date_onset),
             date_end = pmin(as_date(dt_hosp_discharge), date_onset + days(14))) %>%
      
      filter(date_end >= date_start) %>% 
      
      rowwise() %>%
      
      mutate(date = list(seq(date_start, date_end, "days"))) %>%
      unnest(date) %>% 
      
      count(date, name = "hospital"),
    
    hospital_data %>%
      filter(ever_in_icu) %>% 
      mutate(date_start = pmax(as_date(dt_first_icu), date_onset),
             date_end = pmin(as_date(dt_last_icu), date_onset + days(14))) %>%
      
      filter(date_end >= date_start) %>% 
      
      rowwise() %>%
      
      mutate(date = list(seq(date_start, date_end, "days"))) %>%
      unnest(date) %>% 
      
      count(date, name = "ICU"),
    
    by = "date"
  ) %>%
    complete(date = seq(min(date), max(date), "days"),
             fill = list(ICU = 0, hospital = 0)) %>%
    
    mutate(ward = hospital - ICU) %>%
    select(-hospital) %>% 
    
    pivot_longer(c(ward, ICU),
                 names_to = "group",
                 values_to = "count") %>%
    
    mutate(count = as.double(count))
  
  full_occ %>%
    filter(date <= max(date) - days(1))
}




calculate_occupancy_aged <- function(hospital_data) {
  
  full_occ <- left_join(
    hospital_data %>%
      mutate(date_start = pmax(as_date(dt_hosp_admission), date_onset),
             date_end = pmin(as_date(dt_hosp_discharge), date_onset + days(14))) %>%
      
      filter(date_end >= date_start) %>% 
      
      rowwise() %>%
      
      mutate(date = list(seq(date_start, date_end, "days"))) %>%
      unnest(date) %>% 
      
      count(age_group, date, name = "hospital"),
    
    hospital_data %>%
      filter(ever_in_icu) %>% 
      mutate(date_start = pmax(as_date(dt_first_icu), date_onset),
             date_end = pmin(as_date(dt_last_icu), date_onset + days(14))) %>%
      
      filter(date_end >= date_start) %>% 
      
      rowwise() %>%
      
      mutate(date = list(seq(date_start, date_end, "days"))) %>%
      unnest(date) %>% 
      
      count(age_group, date, name = "ICU"),
    
    by = c("date", "age_group")
  ) %>%
    complete(age_group, date = seq(min(date), max(date), "days"),
             fill = list(ICU = 0, hospital = 0)) %>%
    
    mutate(ward = hospital - ICU) %>%
    select(-hospital) %>% 
    
    pivot_longer(c(ward, ICU),
                 names_to = "group",
                 values_to = "count") %>%
    
    mutate(count = as.double(count))
  
  full_occ %>%
    filter(date <= max(date) - days(1))
}

