

plot_results_by_age <- function(
  sim_results, hospital_data_unfiltered,
  
  occupancy_data_aged,
  
  forecast_dates,
  
  plot_dir
) {
  
  source("R/age_groups.R")
  
  
  p_common <- list(
    theme_minimal(),
    geom_vline(xintercept = forecast_dates$forecast_start, linetype = 'dashed'),
    scale_x_date(date_breaks = "months", labels = scales::label_date_short()),
    scale_y_continuous(breaks = scales::extended_breaks(), labels = scales::label_comma()),
    coord_cartesian(xlim = c(ymd("2022-04-01"), NA))
  )
  
  results_aged_transitions_quants <- sim_results$results_aged_transitions_quants
  
  
  count_admit <- hospital_data_unfiltered %>%
    ungroup() %>%
    count(age_group, date = as_date(dt_hosp_admission)) %>%
    
    group_by(age_group) %>%
    mutate(n_smooth = zoo::rollmean(n, 7, fill = NA))
  
  ggplot(results_aged_transitions_quants %>%
           filter(group == "ward")) +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                alpha = 0.2, fill = 'purple') +
    
    geom_point(aes(x= date, y = n),
               size = 0.4,
               count_admit) +
    
    geom_line(aes(x = date, y = n_smooth), count_admit, size = 1) +
    
    facet_wrap(~age_group, scales = "free_y") +
    xlab(NULL) + ylab(NULL) +
    
    ggtitle("Daily ward admissions") +
    
    p_common
  
  ggsave(paste0(plot_dir, "/ages_admission_ward.png"), width = 12, height = 6, bg = "white")
  
  count_discharged <- hospital_data_unfiltered %>%
    filter(!is_still_in_hosp) %>%
    ungroup() %>%
    count(age_group, date = as_date(dt_hosp_discharge)) %>%
    
    group_by(age_group) %>%
    mutate(n_smooth = zoo::rollmean(n, 7, fill = NA))
  
  ggplot(results_aged_transitions_quants %>%
           filter(group == "discharged")) +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                alpha = 0.2, fill = 'blue3') +
    
    geom_point(aes(x= date, y = n),
               size = 0.4,
               count_discharged) +
    
    geom_line(aes(x = date, y = n_smooth), count_discharged, colour = "green") +
    
    facet_wrap(~age_group, scales = "free_y") +
    xlab(NULL) + ylab(NULL) +
    
    ggtitle("Daily discharges") +
    
    p_common
  
  ggsave(paste0(plot_dir, "/ages_discharge.png"), width = 12, height = 6, bg = "white")

  count_died <- hospital_data_unfiltered %>%
    filter(patient_died) %>%
    count(age_group, date = as_date(dt_hosp_discharge)) %>%

    group_by(age_group) %>%
    mutate(n = zoo::rollmean(n, 7, fill = NA))


  ggplot(results_aged_transitions_quants %>%
           filter(group == "died")) +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                alpha = 0.2, fill = ggokabeito::palette_okabe_ito(5))  +

    stat_count(geom = "point", aes(x = as_date(dt_hosp_discharge)),
               size = 0.4,
               hospital_data_unfiltered %>% filter(patient_died)) +

    geom_line(aes(x = date, y = n), size = 0.7, count_died)+

    facet_wrap(~age_group, scales = "free_y") +
    xlab(NULL) + ylab(NULL) +

    ggtitle("Daily deaths") +

    p_common

  ggsave(paste0(plot_dir, "/ages_death.png"), width = 12, height = 6, bg = "white")

  
  ggplot(results_aged_transitions_quants %>%
           filter(group == "ICU")) +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                alpha = 0.2, fill = 'green4') +
    
    stat_count(geom = "point", aes(x = as_date(dt_first_icu)), size = 0.8,
               hospital_data_unfiltered) +
    
    facet_wrap(~age_group, scales = "free_y") +
    xlab(NULL) + ylab(NULL) +
    
    ggtitle("Daily ICU admissions") +
    
    geom_blank(aes(y = 10)) +
    
    p_common
  
  ggsave(paste0(plot_dir, "/ages_admission_ICU.png"), width = 12, height = 6, bg = "white")
  
  
  
  
  
  
  results_aged_quants <- sim_results$results_aged_quants
  
  
  ggplot(results_aged_quants %>%
           filter(group == "ward")) +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                alpha = 0.2, fill = 'purple') +
    
    geom_line(aes(x = date, y = count),
              occupancy_data_aged %>% filter(group == "ward")) +
    
    p_common +
    
    ggtitle("Ward occupancy") +
    
    facet_wrap(~age_group, scales = "free_y") 
  
  ggsave(paste0(plot_dir, "/ages_occupancy_ward.png"), width = 12, height = 6, bg = "white")
  
  ggplot(results_aged_quants %>%
           filter(group == "ICU")) +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                alpha = 0.2, fill = 'green4')  +
    
    geom_line(aes(x = date, y = count),
              occupancy_data_aged %>% filter(group == "ICU")) +
    
    p_common +
    
    ggtitle("ICU occupancy") +
    
    facet_wrap(~age_group) 
  
  
  ggsave(paste0(plot_dir, "/ages_occupancy_ICU.png"), width = 12, height = 6, bg = "white")
  
}
