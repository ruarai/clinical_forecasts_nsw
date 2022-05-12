

plot_time_varying_estimates <- function(
  time_varying_estimates, forecast_dates, plot_dir
) {
  quants_plot <- time_varying_estimates %>%
    pivot_longer(starts_with("pr_")) %>%
    group_by(date, age_group, name) %>%
    summarise(q95_lower = quantile(value, 0.025),
              q95_upper = quantile(value, 0.975),
              q50_lower = quantile(value, 0.25),
              q50_upper = quantile(value, 0.75),
              median = median(value)) %>%
    
    filter(date <= forecast_dates$forecast_start,
           date >= ymd("2022-01-01"))
  
  ggplot(quants_plot %>% filter(name == "pr_age_given_case"))  +
    
    geom_ribbon(aes(x = date, ymin = q95_lower, ymax = q95_upper, fill = age_group),
                alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = q50_lower, ymax = q50_upper, fill = age_group),
                alpha = 0.75) +
    
    geom_line(aes(x = date, y = median, color = age_group),
              alpha = 0.75) +
    
    ggokabeito::scale_fill_okabe_ito(name = "Age group") +
    ggokabeito::scale_color_okabe_ito(name = "Age group") +
    
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(paste0(plot_dir, "/age_distribution.png"), bg = "white", width = 9, height = 6)
  
  
  ggplot(quants_plot %>% filter(name %in% c("pr_ICU", "pr_hosp"))) +
    
    geom_ribbon(aes(x = date, ymin = q95_lower, ymax = q95_upper, fill = name),
                alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = q50_lower, ymax = q50_upper, fill = name),
                alpha = 0.75) +
    
    ggokabeito::scale_fill_okabe_ito() +
    ggokabeito::scale_color_okabe_ito() +
    
    facet_wrap(~name * age_group, ncol = 3, scales = "free_y", labeller = label_wrap_gen(multi_line=FALSE)) +
    
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave(paste0(plot_dir, "/time_varying_estimates.png"), bg = "white", width = 9, height = 9)
  
  
  
}
