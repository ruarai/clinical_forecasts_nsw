library(tidyverse)
library(targets)
library(lubridate)


sim_results <- tar_read(sim_results)

forecast_dates <- tar_read(forecast_dates)


nsw_hospital_path <- tar_read(nsw_hospital_path)


source("../clinical_forecasting/R/age_groups.R")

source("../clinical_forecasting/R/state_data/NSW.R")
hospital_linelist <- read_NSW_linelist(readxl::read_excel(nsw_hospital_path, sheet = 2),
                                       remove_adm_delay = FALSE,
                                       remove_sep_episodes = FALSE) %>%
  mutate(age_group = assign_10yr_age_group(age))

p_common <- list(
  theme_minimal(),
  geom_vline(xintercept = forecast_dates$forecast_start, linetype = 'dashed'),
  scale_x_date(date_breaks = "months", labels = scales::label_date_short()),
  scale_y_continuous(breaks = scales::extended_breaks(), labels = scales::label_comma()),
  coord_cartesian(xlim = c(ymd("2022-03-01"), NA))
)



count_admit <- hospital_linelist %>%
  filter(dt_hosp_admission <= ymd("2022-04-25"),
         #dt_hosp_admission >= ymd("2022-02-03")
         ) %>%
  ungroup() %>%
  count(date = as_date(dt_hosp_admission), name = "transitions_true")


source("../clinical_forecasting/R/progression_model.R")


plot_data_admits <- sim_results$results_ungrouped_formatted %>%
  filter(compartment == "ward") %>% 
  left_join(count_admit) %>%
  drop_na(transitions_true) %>%
  
  group_by(sample) %>%
  
  mutate(diff = transitions - transitions_true,
         diff_cum = zoo::rollsumr(diff, 60, fill = NA),
         diff_cum = cumsum(diff)) %>%
  select(date, sample, diff_cum) %>%
  pivot_wider(names_from = "sample",
              names_prefix = "sim_",
              values_from = "diff_cum") %>%
  make_results_quants()


ggplot(plot_data_admits) +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
              alpha = 0.2, fill = ggokabeito::palette_okabe_ito(5)) +
  
  geom_hline(yintercept = 0) +
  
  
  xlab(NULL) + ylab(NULL) +
  
  ggtitle("Divergence in hospital admissions over rolling 60-day window") +
  
  p_common +
  
  coord_cartesian(xlim = c(ymd("2021-06-01"), NA))



count_discharge <- hospital_linelist %>%
  filter(dt_hosp_admission <= ymd("2022-04-25"),
         #dt_hosp_admission >= ymd("2022-02-03")
         )  %>%
  filter(!is_still_in_hosp) %>%
  ungroup() %>%
  count(date = as_date(dt_hosp_discharge), name = "transitions_true")


plot_data_discharges <- sim_results$results_formatted %>%
  filter(group == "discharged" | group == "died") %>%
  group_by(sample, date) %>%
  summarise(transitions = sum(transitions)) %>%
  left_join(count_admit) %>%
  drop_na(transitions_true) %>%
  
  group_by(sample) %>%
  
  mutate(diff = transitions - transitions_true,
         diff_cum = zoo::rollsumr(diff, 60, fill = NA),
         diff_cum = cumsum(diff)) %>%
  select(date, sample, diff_cum) %>%
  pivot_wider(names_from = "sample",
              names_prefix = "sim_",
              values_from = "diff_cum") %>%
  make_results_quants()



ggplot(plot_data_discharges) +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
              alpha = 0.2, fill = ggokabeito::palette_okabe_ito(3)) +
  xlab(NULL) + ylab(NULL) +
  
  geom_hline(yintercept = 0) +
  
  ggtitle("Divergence in hospital discharges over rolling 60-day window") +
  
  p_common +
  
  coord_cartesian(xlim = c(ymd("2021-06-01"), NA))






