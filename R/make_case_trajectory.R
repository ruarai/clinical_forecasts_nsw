
make_case_trajectory <- function(
  nsw_cases,
  case_forecast,
  forecast_dates,
  plot_dir
) {
  
  nsw_cases_count <- nsw_cases %>%
    count(date_onset = CALCULATED_ONSET_DATE, name = "count") %>%
    filter(date_onset >= forecast_dates$simulation_start,
           date_onset < forecast_dates$forecast_start)
  
  
  case_trajectory_unimputed <- bind_rows(
    case_forecast %>% 
      filter(date_onset > max(nsw_cases_count$date_onset)) %>%
      select(date_onset, count = n), 
    
    nsw_cases_count
  ) %>%
    arrange(date_onset)
  
  case_trajectory <- case_trajectory_unimputed %>%
    complete(date_onset = seq(min(date_onset), max(date_onset), by = "days")) %>%
    
    mutate(count = zoo::na.approx(count))
  
  
  ggplot() +
    geom_point(aes(x = date_onset, y = count),
               case_trajectory_unimputed, size = 0.4) +
    geom_line(aes(x = date_onset, y = count),
              case_trajectory) +
    
    scale_x_date(date_breaks = "months") +
    
    theme_minimal()
  
  ggsave(paste0(plot_dir, "/case_trajectory.png"), bg = "white", height = 5, width = 7)
  
  case_trajectory
}

