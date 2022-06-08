

plot_main_results <- function(
  sim_results, public_occupancy_data,
  forecast_dates,
  
  plot_dir, forecast_name
) {
  results_count_quants <- sim_results$results_count_quants
  
  
  alpha_vals <- scales::rescale(rev(1/1.7^(1:8)), to = c(0.05, 0.99))
  
  ward_base_colour <- "#b53aa0"
  ICU_base_colour <- "#008200"
  
  
  ward_cols <- shades::opacity(ward_base_colour, alpha_vals)
  ICU_cols <- shades::opacity(ICU_base_colour, alpha_vals)
  
  
  
  plots_common <- list(
    scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16)),
    scale_x_date(date_breaks = "months",
                 labels = scales::label_date_short(format = c("%Y", "%B")),
                 expand = expansion(mult = c(0.01, 0.05))),
    scale_y_continuous(breaks = scales::extended_breaks(),
                       labels = scales::label_comma(),
                       expand = expansion(mult = c(0, 0.1)),
                       sec.axis = dup_axis(name = "")),
    geom_blank(aes(y = 0)), geom_blank(aes(y = 30)),
    xlab(NULL), ylab("Count"),
    theme_minimal(),
    theme(legend.position = "none",
          panel.grid = element_blank(),
          panel.grid.major = element_line(colour = "white", linetype = "dotted"),
          axis.ticks = element_line(colour = "grey60"),
          axis.ticks.length = unit(5, "pt"),
          axis.line = element_line(colour = "grey40"),
          plot.title = element_text(size = 15),
          axis.text = element_text(size = 12),
          axis.title.y = element_blank(),
          text = element_text(family = "Helvetica")),
    coord_cartesian(xlim = c(ymd("2021-12-01"), NA))
  )
  
  p_ward <- ggplot(results_count_quants %>%
                     filter(group == "ward", date >= ymd("2022-03-01"))) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant)) +
    
    scale_fill_manual(values = ward_cols) +
    
    geom_point(aes(x = date, y = count),
               public_occupancy_data %>%
                  filter(group == "ward"),
               pch = 1,
               color = "grey20",
               
               size = 1, stroke = 0.4) +
    
    geom_vline(aes(xintercept = forecast_dates$forecast_start), linetype = 'dashed') +
    
    plots_common +
    
    xlab(NULL) + ylab("Count") +
    
    ggtitle("Ward beds occupied")
  
  
  p_ICU <- ggplot(results_count_quants %>%
                    filter(group == "ICU", date >= ymd("2022-03-01"))) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant)) +
    
    scale_fill_manual(values = ICU_cols)  +
    
    geom_point(aes(x = date, y = count),
               public_occupancy_data %>%
                  filter(group == "ICU"),
               pch = 1,
               color = "grey20",
               
               size = 1, stroke = 0.4) +
    
    geom_vline(aes(xintercept = forecast_dates$forecast_start), linetype = 'dashed') +
    
    plots_common +
    
    xlab(NULL) + ylab("Count") +
    
    ggtitle("ICU beds occupied")
  
  cowplot::plot_grid(
    p_ward,
    p_ICU,
    ncol = 1,
    align = "hv", axis = "lr"
  )
  
  
  ggsave(paste0(plot_dir, "/", forecast_name, "_estimates.png"), width = 10, height = 8, bg = "white")
  
  
}

