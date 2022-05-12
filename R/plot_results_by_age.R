

plot_results_by_age <- function(
  sim_results, hospital_data_unfiltered,
  
  occupancy_by_age_ward_path, occupancy_by_age_ICU_path,
  
  forecast_dates,
  
  plot_dir
) {
  
  source("../clinical_forecasting/R/age_groups.R")
  
  
  p_common <- list(
    theme_minimal(),
    geom_vline(xintercept = forecast_dates$forecast_start, linetype = 'dashed'),
    scale_x_date(date_breaks = "months", labels = scales::label_date_short()),
    scale_y_continuous(breaks = scales::extended_breaks(), labels = scales::label_comma()),
    coord_cartesian(xlim = c(ymd("2022-03-01"), NA))
  )
  
  results_aged_transitions_quants <- sim_results$results_aged_transitions_quants
  
  
  count_admit <- hospital_data_unfiltered %>%
    ungroup() %>%
    count(age_group, date = as_date(dt_hosp_admission)) %>%
    
    group_by(age_group) %>%
    mutate(n = zoo::rollmean(n, 7, fill = NA))
  
  ggplot(results_aged_transitions_quants %>%
           filter(group == "ward")) +
    
    stat_count(geom = "point", aes(x = as_date(dt_hosp_admission)), size = 0.8,
               hospital_data_unfiltered) +
    
    geom_line(aes(x = date, y = n), count_admit) +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                alpha = 0.2, fill = 'purple') +
    
    facet_wrap(~age_group, scales = "free_y") +
    xlab(NULL) + ylab(NULL) +
    
    ggtitle("Daily ward admissions") +
    
    p_common
  
  ggsave(paste0(plot_dir, "/ages_admission_ward.png"), width = 8, height = 6, bg = "white")
  
  count_discharged <- hospital_data_unfiltered %>%
    filter(!is_still_in_hosp) %>%
    ungroup() %>%
    count(age_group, date = as_date(dt_hosp_discharge)) %>%
    
    group_by(age_group) %>%
    mutate(n = zoo::rollmean(n, 7, fill = NA))
  
  ggplot(results_aged_transitions_quants %>%
           filter(group == "discharged")) +
    
    stat_count(geom = "point", aes(x = as_date(dt_hosp_discharge)), size = 0.8,
               hospital_data_unfiltered %>% filter(!is_still_in_hosp)) +
    
    geom_line(aes(x = date, y = n), count_discharged) +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                alpha = 0.2, fill = ggokabeito::palette_okabe_ito(7)) +
    
    facet_wrap(~age_group, scales = "free_y") +
    xlab(NULL) + ylab(NULL) +
    
    ggtitle("Daily discharges") +
    
    p_common
  
  ggsave(paste0(plot_dir, "/ages_discharge.png"), width = 8, height = 6, bg = "white")
  # 
  # 
  # did_patient_die <- function(discharge_description) {
  #   death_descriptors <- c("deceased", "death", "died", "dead")
  #   regex_match <- str_c("(?i)(", str_c(death_descriptors, collapse = "|"), ")")
  #   
  #   str_detect(discharge_description, regex_match)
  # }
  # 
  # linelist_died <- linelist_raw_NSW %>%
  #   mutate(died = did_patient_die(AP_DISCHARGE_DISPOSITION_DESC)) %>%
  #   filter(died)
  # 
  # count_died <- linelist_died %>%
  #   count(age_group, date = as_date(discharge_date)) %>%
  #   
  #   group_by(age_group) %>%
  #   mutate(n = zoo::rollmean(n, 7, fill = NA))
  # 
  # 
  # ggplot(results_aged_transitions_quants %>%
  #          filter(group == "died")) +
  #   
  #   stat_count(geom = "point", aes(x = as_date(discharge_date)), size = 0.8,
  #              linelist_died) +
  #   
  #   geom_line(aes(x = date, y = n), count_died) +
  #   
  #   geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
  #               alpha = 0.2, fill = ggokabeito::palette_okabe_ito(5)) +
  #   
  #   facet_wrap(~age_group, scales = "free_y") +
  #   xlab(NULL) + ylab(NULL) +
  #   
  #   ggtitle("Daily deaths") +
  #   
  #   p_common
  # 
  # ggsave(paste0(plot_dir, "/ages_death.png"), width = 8, height = 6, bg = "white")
  
  
  ggplot(results_aged_transitions_quants %>%
           filter(group == "ICU")) +
    
    stat_count(geom = "point", aes(x = as_date(dt_first_icu)), size = 0.8,
               hospital_data_unfiltered) +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                alpha = 0.2, fill = 'green4') +
    
    facet_wrap(~age_group, scales = "free_y") +
    xlab(NULL) + ylab(NULL) +
    
    ggtitle("Daily ICU admissions") +
    
    geom_blank(aes(y = 10)) +
    
    p_common
  
  ggsave(paste0(plot_dir, "/ages_admission_ICU.png"), width = 8, height = 6, bg = "white")
  
  
  
  
  
  ward_occupancy <- read_csv(occupancy_by_age_ward_path) %>%
    select(date = DATE, date_snapshot = SNAPSHOT_DATE,
           
           age_group = AGE_GROUP_10YR, count_PCR = PCR_Ward, count_RAT = RAT_Ward) %>%
    pivot_longer(cols = c(count_PCR, count_RAT),
                 names_prefix = "count_",
                 names_to = "type", values_to = "count")
  
  ICU_occupancy <- read_csv(occupancy_by_age_ICU_path) %>%
    select(date = DATE, date_snapshot = SNAPSHOT_DATE,
           
           age_group = AGE_GROUP_10YR, count_PCR = PCR_ICU, count_RAT = RAT_ICU) %>%
    pivot_longer(cols = c(count_PCR, count_RAT),
                 names_prefix = "count_",
                 names_to = "type", values_to = "count")
  
  align_age_groups <- function(x) {
    x <- x %>% str_remove(" years")
    
    case_when(x == "80-89" ~ "80+", x == "90+" ~ "80+", TRUE ~ x)
  } 
  
  
  all_occupancy <- bind_rows(
    ward_occupancy %>% mutate(group = "ward"),
    ICU_occupancy %>% mutate(group = "ICU")
  ) %>%
    mutate(age_group = align_age_groups(age_group)) %>%
    group_by(date, age_group, group) %>%
    summarise(count = sum(count))
  
  results_aged_quants <- sim_results$results_aged_quants
  
  
  ggplot(results_aged_quants %>%
           filter(group == "ward")) +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                alpha = 0.2, fill = 'purple') +
    
    geom_line(aes(x = date, y = count),
              data = all_occupancy %>% filter(group == "ward")) +
    
    p_common +
    
    ggtitle("Ward occupancy") +
    
    facet_wrap(~age_group) 
  
  ggsave(paste0(plot_dir, "/ages_occupancy_ward.png"), width = 8, height = 6, bg = "white")
  
  ggplot(results_aged_quants %>%
           filter(group == "ICU")) +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                alpha = 0.2, fill = 'green4') +
    
    geom_line(aes(x = date, y = count),
              data = all_occupancy %>% filter(group == "ICU")) +
    
    p_common +
    
    ggtitle("ICU occupancy") +
    
    facet_wrap(~age_group) 
  
  
  ggsave(paste0(plot_dir, "/ages_occupancy_ICU.png"), width = 8, height = 6, bg = "white")
  
}
