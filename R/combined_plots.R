

library(lubridate)
library(tidyverse)
library(targets)

forecast_dates <- tar_read(forecast_dates)


public_occupancy_data <- tar_read(occupancy_data) %>%
  filter(date <= forecast_dates$forecast_start + ddays(4))

combined_name <- "2022-11-07_scenarios"
forecast_names <- c(
  "fc_2022-11-07_final_1",
  "fc_2022-11-07_final_2",
  "fc_2022-11-07_final_3"
)


forecast_labels <- c("Mid", "Low", "High") %>% `names<-`(forecast_names)

quant_files <- str_c("results/", forecast_names, "/", forecast_names, "_result_summaries.csv")


quants <- map_dfr(quant_files, read_csv, show_col_types = FALSE, .id = "forecast_name") %>%
  filter(date <= forecast_dates$forecast_start + ddays(60))

plot_data <- quants %>%
  mutate(forecast_name = forecast_names[as.numeric(forecast_name)]) %>%
  mutate(forecast_name = factor(forecast_name, levels = forecast_names))



alpha_vals <- scales::rescale(rev(1/1.7^(1:3)), to = c(0.05, 0.99))

ward_base_colour <- "#b53aa0"
ICU_base_colour <- "#008200"


ward_cols <- shades::opacity(ward_base_colour, alpha_vals)
ICU_cols <- shades::opacity(ICU_base_colour, alpha_vals)




plots_common <- list(
  scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16)),
  scale_x_date(date_breaks = "months",
               labels = scales::label_date_short(format = c("%Y", "%b")),
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
  coord_cartesian(xlim = c(ymd("2021-12-01"), forecast_dates$forecast_start + ddays(60)))
)


p_ward <- ggplot(plot_data %>%
         filter(date >= forecast_dates$forecast_start)) +
  geom_ribbon(aes(x = date, ymin = ward_lower90, ymax = ward_upper90, fill = forecast_name),
              alpha = 0.6) +
  
  #geom_line(aes(x = date, y = ward_median, color = forecast_name)) +
  
  ggokabeito::scale_color_okabe_ito(name = "", labels = forecast_labels) +
  ggokabeito::scale_fill_okabe_ito(name = "", labels = forecast_labels) +
  
  geom_vline(aes(xintercept = forecast_dates$forecast_start), linetype = 'dashed',
             color = 'grey50') +

  
  geom_point(aes(x = date, y = count),
             public_occupancy_data %>%
               filter(group == "ward"),
             color = "grey20",
             
             size = 0.6, stroke = 0.2) +
  
  plots_common +
  
  xlab(NULL) + ylab("Count") +
  
  ggtitle("Ward beds occupied")


p_ICU <- ggplot(plot_data %>%
                   filter(date >= forecast_dates$forecast_start)) +
  geom_ribbon(aes(x = date, ymin = ICU_lower90, ymax = ICU_upper90, fill = forecast_name),
              alpha = 0.6) +

  #geom_line(aes(x = date, y = ICU_median, color = forecast_name)) +

  
  ggokabeito::scale_color_okabe_ito(name = "", labels = forecast_labels) +
  ggokabeito::scale_fill_okabe_ito(name = "", labels = forecast_labels) +
  
  geom_vline(aes(xintercept = forecast_dates$forecast_start), linetype = 'dashed',
             color = 'grey50') +
  
  
  geom_point(aes(x = date, y = count),
             public_occupancy_data %>%
               filter(group == "ICU"),
             color = "grey20",
             
             size = 0.6, stroke = 0.2) +
  
  plots_common +
  
  xlab(NULL) + ylab("Count") +
  
  ggtitle("ICU beds occupied")


cowplot::plot_grid(
  cowplot::plot_grid(
    p_ward,
    p_ICU,
    ncol = 1,
    align = "hv", axis = "lr"
  ),
  cowplot::get_legend(p_ICU + theme(legend.position = "bottom")),
  ncol = 1,
  rel_heights = c(12, 1)
)

ggsave(paste0("results/", combined_name, "_estimates.png"), width = 10, height = 8, bg = "white")
