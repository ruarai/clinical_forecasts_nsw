


forecast_dates <- tar_read(forecast_dates)
hospital_data <- tar_read(hospital_data_unfiltered)


combined_name <- "2022-11-15_scenarios"
forecast_names <- c(
  "fc_2022-11-15_final_1",
  "fc_2022-11-15_final_2",
  "fc_2022-11-15_final_3"
)

forecast_labels <- c("Mid", "Low", "High") %>% `names<-`(forecast_names)
results_files <- str_c("results/", forecast_names, "/sim_results.qs")


results_ls <- map(results_files, qs::qread) %>% `names<-`(forecast_names)

trans_quants <- map_dfr(results_ls, function(x) x$results_ungrouped_transitions_quants, .id = "forecast_name")
occ_quants <- map_dfr(results_ls, function(x) x$results_count_quants, .id = "forecast_name")


plot_data_trans_quants <- trans_quants %>%
  filter(compartment == "ward", quant %in% c(30, 50, 70)) %>% 
  mutate(forecast_name = forecast_labels[forecast_name]) %>%
  mutate(forecast_name = factor(forecast_name, levels = forecast_labels))


admission_counts <- hospital_data %>%
  count(date = as_date(dt_hosp_admission)) %>%
  complete(date = seq(min(date), max(date), "days"), fill = list(n = 0)) %>%
  arrange(date) %>%
  mutate(n_smooth = zoo::rollmean(n, k = 7, fill = NA))


alpha_vals <- scales::rescale(rev(1/1.7^(1:3)), to = c(0.15, 0.99))
plot_cols <- c(
  shades::opacity(ggokabeito::palette_okabe_ito(1), alpha_vals), 
  shades::opacity(ggokabeito::palette_okabe_ito(2), alpha_vals), 
  shades::opacity(ggokabeito::palette_okabe_ito(3), alpha_vals)
)


date_plot_start <- ymd("2022-05-01")
p_common <- list(
  scale_fill_manual(values = plot_cols),
  geom_vline(xintercept = forecast_dates$forecast_start, linetype = "dashed"),
  xlab(NULL), ylab(NULL),
  theme_minimal(), theme(legend.position = "none"),
  scale_y_continuous(breaks = scales::breaks_extended(4), labels = scales::label_comma()),
  scale_x_date(breaks = scales::date_breaks("months"), labels = scales::label_date_short())
)

p_trans <- ggplot() +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = interaction(quant, forecast_name)),
              plot_data_trans_quants) +
  
  geom_point(aes(x = date, y = n),
             pch = 1, size = 0.9, stroke = 0.7,
             admission_counts) +
  
  geom_line(aes(x = date, y = n_smooth),
            linetype = "solid",
            admission_counts) +
  
  
  geom_vline(xintercept = forecast_dates$forecast_start, linetype = "dashed") +
  coord_cartesian(xlim = c(date_plot_start, forecast_dates$forecast_start + days(60)),
                  ylim = c(0, 300)) +
  
  ggtitle("Daily ward admissions") +
  
  p_common
  
  



plot_data_occ_quants <- occ_quants %>%
  filter(group == "ward", quant %in% c(30, 50, 70)) %>% 
  mutate(forecast_name = forecast_labels[forecast_name]) %>%
  mutate(forecast_name = factor(forecast_name, levels = forecast_labels))


occupancy_counts <- tar_read(occupancy_data) %>%
  filter(group == "ward")

p_occ <- ggplot() +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = interaction(quant, forecast_name)),
              plot_data_occ_quants) +
  
  geom_point(aes(x = date, y = count),
             pch = 1, size = 0.9, stroke = 0.7,
             occupancy_counts) +
  
  geom_vline(xintercept = forecast_dates$forecast_start, linetype = "dashed") +
  coord_cartesian(xlim = c(date_plot_start, forecast_dates$forecast_start + days(60)),
                  ylim = c(0, 1800)) +
  
  ggtitle("Daily ward occupancy") +
  
  p_common


cowplot::plot_grid(
  cowplot::plot_grid(
    p_trans,
    p_occ,
    ncol = 1,
    align = "hv", axis = "lr"
  ),
  cowplot::get_legend(
    ggplot(plot_data_trans_quants %>% filter(quant == 50)) + 
      geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = forecast_name)) +
      scale_fill_manual(values = ggokabeito::palette_okabe_ito(1:3), labels = forecast_labels, name = NULL) + 
      theme(legend.position = "bottom")
  ),
  ncol = 1,
  rel_heights = c(12, 1)
)

ggsave(paste0("results/", combined_name, "_estimates_admissions.png"), width = 10, height = 8, bg = "white")










