
make_case_trajectory <- function(
  nsw_cases,
  case_forecast,
  forecast_dates,
  plot_dir
) {
  
  # Count the number of detected cases for each onset date, bounded by our simulation and forecast dates
  nsw_cases_count <- nsw_cases %>%
    count(date_onset = CALCULATED_ONSET_DATE, name = "count") %>%
    complete(date_onset = seq(forecast_dates$simulation_start, forecast_dates$forecast_start - ddays(1), by = "days"), fill = list(count = 0)) %>%
    filter(date_onset >= forecast_dates$simulation_start,
           date_onset < forecast_dates$forecast_start)
  
  # Combined the count into the backcast with that of the forecast
  case_trajectory_unimputed <- bind_rows(
    case_forecast %>% 
      filter(date_onset > max(nsw_cases_count$date_onset)) %>%
      select(date_onset, count = n), 
    
    nsw_cases_count
  ) %>%
    arrange(date_onset) %>%
    
    # left_join(get_neuts_change(.$date_onset) %>%
    #             select(date, proportionBA4BA5),
    #           by = c("date_onset" = "date")) %>%
    
    mutate(count = round(count))
  
  # Make sure our trajectory is complete, imputing missing values with simple linear interpolation
  # This might cause issues if there's a large enough gap between the backcast and forecast, so be careful with this
  case_trajectory <- case_trajectory_unimputed %>%
    complete(date_onset = seq(min(date_onset), max(date_onset), by = "days")) %>%
    
    mutate(count = zoo::na.approx(count))
  
  # Produce a plot of the case trajectory
  # If points are missing along the trajectory in the plot, this is where imputation has occurred
  ggplot() +
    geom_point(aes(x = date_onset, y = count),
               case_trajectory_unimputed, size = 0.4) +
    geom_line(aes(x = date_onset, y = count),
              case_trajectory) +
    
    scale_x_date(date_breaks = "months", labels = scales::label_date_short()) +
    
    theme_minimal()
  
  ggsave(paste0(plot_dir, "/case_trajectory.png"), bg = "white", height = 5, width = 7)
  
  case_trajectory
}

