

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
              median = median(value), .groups = "drop") %>%
    
    filter(date >= ymd("2022-01-01"))
  
  age_plot <- quants_plot %>% 
    filter(name == "pr_age_given_case")
  
  ggplot(age_plot)  +
    
    geom_ribbon(aes(x = date, ymin = q95_lower, ymax = q95_upper, fill = age_group),
                alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = q50_lower, ymax = q50_upper, fill = age_group),
                alpha = 0.75) +
    
    geom_line(aes(x = date, y = median, color = age_group),
              alpha = 0.75)  +
    
    geom_line(aes(x = date, y = median, color = age_group),
              alpha = 0.2,
              quants_plot %>% filter(name == "pr_age_given_case")) +
    
    geom_vline(xintercept = forecast_dates$forecast_start, alpha = 0.5, linetype = "dashed") +
    
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
    
    geom_vline(xintercept = forecast_dates$forecast_start, alpha = 0.5, linetype = "dashed") +
    
    facet_wrap(~name * age_group, ncol = 3, scales = "free_y", labeller = label_wrap_gen(multi_line=FALSE)) +
    
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave(paste0(plot_dir, "/time_varying_estimates.png"), bg = "white", width = 9, height = 9)
  
  
  
  age_adj_plot <- quants_plot %>% 
    filter(name %in% c("pr_age_given_case", "pr_age_old")) %>%
    mutate(name = if_else(name == "pr_age_old", "old", "adjusted"))
  
  
  ggplot(age_adj_plot %>% filter(date >= forecast_dates$forecast_start - ddays(60)))  +
    # 
    # geom_ribbon(aes(x = date, ymin = q95_lower, ymax = q95_upper, fill = age_group),
    #             alpha = 0.5) +
    # geom_ribbon(aes(x = date, ymin = q50_lower, ymax = q50_upper, fill = age_group),
    #             alpha = 0.75) +
    
    geom_line(aes(x = date, y = median, color = name),
              alpha = 1)  +
    
    facet_wrap(~age_group) +
    
    geom_vline(xintercept = forecast_dates$forecast_start, alpha = 0.5, linetype = "dashed") +
    
    ggokabeito::scale_fill_okabe_ito(name = "Age group") +
    ggokabeito::scale_color_okabe_ito(name = "Age group") +
    
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(paste0(plot_dir, "/age_distribution_adj.png"), bg = "white", width = 9, height = 6)
  
  adj_plot <- quants_plot %>% 
    filter(name %in% c("pr_hosp", "pr_hosp_old")) %>%
    mutate(name = if_else(name == "pr_hosp_old", "old", "adjusted"))
  
  ggplot(adj_plot) +
    
    geom_ribbon(aes(x = date, ymin = q95_lower, ymax = q95_upper, fill = name),
                alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = q50_lower, ymax = q50_upper, fill = name),
                alpha = 0.75) +
    
    ggokabeito::scale_fill_okabe_ito() +
    ggokabeito::scale_color_okabe_ito() +
    
    geom_vline(xintercept = forecast_dates$forecast_start, alpha = 0.5, linetype = "dashed") +
    
    facet_wrap(~age_group, ncol = 3, scales = "free_y", labeller = label_wrap_gen(multi_line=FALSE)) +
    
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(paste0(plot_dir, "/time_varying_estimates_adj.png"), bg = "white", width = 9, height = 9)
  
  
}
