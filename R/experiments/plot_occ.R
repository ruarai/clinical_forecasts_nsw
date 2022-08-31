
library(targets)
library(tidyverse)
library(lubridate)

sim_results <- tar_read(sim_results)
public_occupancy_data <- tar_read(public_occupancy_data)


forecast_dates <- tar_read(forecast_dates)

hospital_data_unfiltered <- tar_read(hospital_data_unfiltered)



results_count_quants <- sim_results$results_count_quants

alpha_vals <- scales::rescale(rev(1/1.7^(1:8)), to = c(0.05, 0.99))

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
  coord_cartesian(xlim = c(ymd("2021-07-01"), NA))
)


ll_raw <- readxl::read_excel("~/source/email_digester/downloads/hospital_linelist/NSW_out_episode_2022_08_15.xlsx", sheet = 2)

source("../los_rates/R/process_NSW_data.R")

ll_processing <- read_NSW_linelist(ll_raw, FALSE, FALSE, FALSE)
ll_processed <- ll_processing$data


obs_occ_ll <- tibble(
  date = seq(ymd("2021-07-01"), ymd("2022-08-15"), "day")
) %>%
  rowwise() %>% 
  mutate(
    n_ward = ll_processed %>%
      filter(dt_hosp_admission <= date, dt_hosp_discharge >= date | is.na(dt_hosp_discharge),
             dt_first_icu >= date | is.na(dt_first_icu), dt_last_icu <= date | is.na(dt_last_icu)) %>%
      nrow(),
    n_ICU = ll_processed %>%
      filter(dt_first_icu <= date, dt_last_icu >= date) %>%
      nrow()
  )


plot_obs_occ_ll <- obs_occ_ll %>%
  mutate(censored = date >= ymd("2022-07-15"))

p_ward <- ggplot(results_count_quants %>%
                   filter(group == "ward", date >= ymd("2021-01-01"))) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant)) +

  
  scale_fill_manual(values = ward_cols) +
  
  geom_line(aes(x = date, y = n_ward),
            plot_obs_occ_ll,
            color = "grey20",
            
            size = 0.8) +
  
  geom_line(aes(x = date, y = count),
            public_occupancy_data %>%
              filter(group == "ward"),
            color = "grey50",
            
            size = 0.5) +
  
  geom_vline(aes(xintercept = forecast_dates$forecast_start), linetype = 'dashed') +
  
  plots_common +
  
  xlab(NULL) + ylab("Count") +
  
  ggtitle("Ward beds occupied")

p_ward

p_ICU <- ggplot(results_count_quants %>%
                  filter(group == "ICU", date >= ymd("2021-06-01"))) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant)) +
  
  scale_fill_manual(values = ICU_cols) +
  
  geom_line(aes(x = date, y = n_ICU),
            plot_obs_occ_ll,
            color = "grey20",
            
            size = 0.8) +
  
  geom_line(aes(x = date, y = count),
            public_occupancy_data %>%
              filter(group == "ICU"),
            color = "grey50",
            
            size = 0.5) +
  
  
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

