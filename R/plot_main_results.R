

plot_main_results <- function(
  sim_results,
  forecast_dates,
  
  plot_dir, forecast_name
) {
  results_count_quants <- sim_results$results_count_quants
  
  
  true_occupancy_curve <- read_csv("https://github.com/M3IT/COVID-19_Data/raw/master/Data/COVID_AU_state.csv",
                                   show_col_types = FALSE) %>%
    select(-state) %>%
    rename(state = state_abbrev) %>%
    
    mutate(ward_cum = hosp_cum - icu_cum) %>%
    select(state, date, ward = ward_cum, ICU = icu_cum) %>%
    
    pivot_longer(cols = -c(state, date),
                 values_to = "count", names_to = "group") %>%
    
    filter(state == "NSW") %>%
    
    filter(date >= forecast_dates$simulation_start)
  
  p_ward <- ggplot(results_count_quants %>%
                     filter(group == "ward")) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                fill = 'purple', alpha = 0.2) +
    
    geom_line(aes(x = date, y = count),
              true_occupancy_curve %>%
                filter(group == "ward")) +
    
    geom_vline(aes(xintercept = forecast_dates$forecast_start), linetype = 'dashed') +
    
    scale_x_date(
      date_breaks = "months",
      labels = scales::label_date_short()
    ) +
    
    coord_cartesian(xlim = c(ymd("2022-03-01"), NA)) +
    
    xlab(NULL) + ylab("Count") +
    
    ggtitle(NULL, "Ward beds occupied") +
    
    theme_minimal()
  
  
  p_ICU <- ggplot(results_count_quants %>%
                    filter(group == "ICU")) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                fill = 'green4', alpha = 0.2) +
    
    geom_line(aes(x = date, y = count),
              true_occupancy_curve %>%
                filter(group == "ICU")) +
    
    geom_vline(aes(xintercept = forecast_dates$forecast_start), linetype = 'dashed') +
    
    scale_x_date(
      date_breaks = "months",
      labels = scales::label_date_short()
    ) +
    
    coord_cartesian(xlim = c(ymd("2022-03-01"), NA)) +
    
    xlab(NULL) + ylab("Count") +
    
    ggtitle(NULL, "ICU beds occupied") +
    
    theme_minimal()
  
  cowplot::plot_grid(
    p_ward,
    p_ICU,
    ncol = 1,
    align = "hv", axis = "lr"
  )
  
  
  ggsave(paste0(plot_dir, "/", forecast_name, "_estimates.png"), width = 10, height = 8, bg = "white")
  
  
}

