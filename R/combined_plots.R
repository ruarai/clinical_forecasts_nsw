

library(lubridate)
library(tidyverse)

public_occupancy_data <- tar_read(public_occupancy_data)
forecast_dates <- tar_read(forecast_dates)

combined_name <- "2022-06-07_combined"
forecast_names <- c(
  "fc_2022-06-07_Lo_final",
  "fc_2022-06-07_Ba_final",
  "fc_2022-06-07_Hi_final"
)
forecast_labels <- c("Low", "Baseline", "High") %>% `names<-`(forecast_names)

quant_files <- str_c("results/", forecast_names, "/", forecast_names, "_result_summaries.csv")


quants <- map_dfr(quant_files, read_csv, show_col_types = FALSE, .id = "forecast_name")

plot_data <- quants %>%
  mutate(forecast_name = forecast_names[as.numeric(forecast_name)])



alpha_vals <- scales::rescale(rev(1/1.7^(1:3)), to = c(0.05, 0.99))

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


p_ward <- ggplot(plot_data %>%
         filter(date >= forecast_dates$forecast_start)) +
  geom_ribbon(aes(x = date, ymin = ward_lower90, ymax = ward_upper90, fill = forecast_name),
              alpha = 0.6) +
  
  geom_line(aes(x = date, y = ward_median, color = forecast_name)) +
  
  ggokabeito::scale_color_okabe_ito(name = "", labels = forecast_labels) +
  ggokabeito::scale_fill_okabe_ito(name = "", labels = forecast_labels) +

  
  geom_point(aes(x = date, y = count),
             public_occupancy_data %>%
               filter(group == "ward"),
             pch = 1,
             color = "grey20",
             
             size = 1, stroke = 0.4) +
  
  geom_vline(aes(xintercept = forecast_dates$forecast_start), linetype = 'dashed') +
  
  plots_common +
  
  xlab(NULL) + ylab("Count") +
  
  ggtitle("Ward beds occupied") +
  theme(legend.position = "bottom")


p_ICU <- ggplot(plot_data %>%
                   filter(date >= forecast_dates$forecast_start)) +
  geom_ribbon(aes(x = date, ymin = ICU_lower90, ymax = ICU_upper90, fill = forecast_name),
              alpha = 0.6) +
  
  geom_line(aes(x = date, y = ICU_median, color = forecast_name)) +
  
  ggokabeito::scale_color_okabe_ito(name = "", labels = forecast_labels) +
  ggokabeito::scale_fill_okabe_ito(name = "", labels = forecast_labels) +
  
  
  geom_point(aes(x = date, y = count),
             public_occupancy_data %>%
               filter(group == "ICU"),
             pch = 1,
             color = "grey20",
             
             size = 1, stroke = 0.4) +
  
  geom_vline(aes(xintercept = forecast_dates$forecast_start), linetype = 'dashed') +
  
  plots_common +
  
  xlab(NULL) + ylab("Count") +
  
  ggtitle("ICU beds occupied") +
  theme(legend.position = "bottom")



cowplot::plot_grid(
  p_ward,
  p_ICU,
  ncol = 1,
  align = "hv", axis = "lr"
)


ggsave(paste0("results/", combined_name, "_estimates.png"), width = 10, height = 8, bg = "white")
