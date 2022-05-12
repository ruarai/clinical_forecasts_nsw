

plot_main_results <- function(
  sim_results, public_occupancy_data,
  forecast_dates,
  
  plot_dir, forecast_name
) {
  results_count_quants <- sim_results$results_count_quants
  
  
  p_ward <- ggplot(results_count_quants %>%
                     filter(group == "ward")) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                fill = 'purple', alpha = 0.2) +
    
    geom_line(aes(x = date, y = count),
              public_occupancy_data %>%
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
              public_occupancy_data %>%
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

