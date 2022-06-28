quants_plot <- tar_read(time_varying_estimates_adj) %>%
  pivot_longer(starts_with("pr_")) %>%
  group_by(date, age_group, name) %>%
  summarise(q95_lower = quantile(value, 0.025),
            q95_upper = quantile(value, 0.975),
            q50_lower = quantile(value, 0.25),
            q50_upper = quantile(value, 0.75),
            median = median(value), .groups = "drop") %>%
  
  filter(date >= ymd("2022-01-01"))
