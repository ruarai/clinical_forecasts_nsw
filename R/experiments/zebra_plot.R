
library(targets)
library(tidyverse)
library(lubridate)


hospital_data_unfiltered <- tar_read(hospital_data_unfiltered)


hosp_subset <- hospital_data_unfiltered %>%
  filter(dt_hosp_admission >= ymd("2021-12-01"))



hosp_subset %>%
  count(date = as_date(dt_hosp_discharge),
        month_admit = floor_date(dt_hosp_admission, "month"),
        name = "n_discharge") %>%
  
  ggplot() +
  
  geom_line(aes(x = date, y = n_discharge, group = month_admit))


hosp_by_month <- hosp_subset %>%
  count(date = as_date(dt_hosp_admission),
        month_admit = as_date(floor_date(dt_hosp_admission, "month")),
        name = "n_admit") %>%
  
  complete(month_admit,
           date = seq(min(date), max(date), "days"),
           fill = list(n_admit = 0)) %>%
  
  left_join(
    hosp_subset %>%
      count(date = as_date(dt_hosp_discharge),
            month_admit = as_date(floor_date(dt_hosp_admission, "month")),
            name = "n_discharge")
  ) %>%
  
  mutate(n_discharge = replace_na(n_discharge, 0)) %>% 
  group_by(month_admit) %>%
  arrange(date) %>%
  mutate(n_cum = cumsum(n_admit - n_discharge)) %>%
  
  ungroup()


hosp_by_month %>%
  
  group_by(date) %>%
  mutate(n_cum_stack = cumsum(n_cum),
         n_cum_stack_bottom = lag(n_cum_stack, default = 0)) %>%
  
  filter(date >= month_admit - ddays(1)) %>%
  
  ggplot() +
  geom_ribbon(aes(x = date, ymin = n_cum_stack_bottom, ymax = n_cum_stack, group = month_admit, fill = factor(month_admit))) +
  
  theme_minimal() +
  
  ggokabeito::scale_fill_okabe_ito(order = c(6,2,3,5,6,2,3,5,6,2,3,5)) +
  
  theme(legend.position = "none")



hosp_by_month %>%
  
  filter(date >= month_admit - ddays(1)) %>%
  
  ggplot() +
  geom_ribbon(aes(x = date, ymin = 0, ymax = n_cum, group = month_admit, fill = factor(month_admit))) +
  
  facet_wrap(~month_admit, ncol = 2) +
  
  theme_minimal() +
  
  ggokabeito::scale_fill_okabe_ito(order = c(6,2,3,5,6,2,3,5,6,2,3,5)) +
  
  theme(legend.position = "none")





hosp_raw <- readxl::read_xlsx("~/source/email_digester/downloads/hospital_linelist/NSW_out_episode_2022_09_05.xlsx",
                              sheet = 2) %>%
  
  filter(admit_date_dt >= ymd("2021-12-01"))


cases <- tar_read(nsw_cases)

source("R/age_groups.R")

case_count <- cases %>%
  filter(AGE_AT_EVENT_YEARS >= 50) %>%
  mutate(age_group = assign_10yr_age_group(AGE_AT_EVENT_YEARS)) %>%
  count(date_onset = CALCULATED_ONSET_DATE,
        age_group,
        name = "n_cases") %>%
  group_by(age_group) %>%
  
  mutate(n_cases = zoo::rollsum(n_cases, k= 14, fill = NA)) %>%
  ungroup() %>%
  
  complete(date_onset, age_group, fill = list(n_cases = 0))#  %>%
  
  #filter(date_onset >= ymd("2022-03-01"))


source("../los_rates/R/get_admission_reason.R")

hosp_raw_reason <- hosp_raw %>%
  get_admission_reason()


hosp_raw_ind <- hosp_raw_reason %>%
  arrange(admit_date_dt) %>%
  group_by(person_id) %>%
  slice(1) %>%
  ungroup()

plot_data <- hosp_raw_ind %>%
  
  filter(age >= 50) %>%
  mutate(age_group = assign_10yr_age_group(age)) %>%
  
  mutate(ADMISSION_REASON = replace_na(ADMISSION_REASON, ""),
         SUB_WARD_TYPE = replace_na(SUB_WARD_TYPE, "")) %>%
  
  count(date = as_date(admit_date_dt - ddays(covid_to_adm)),
        ward = SUB_WARD_TYPE == "Dialysis",#str_detect(ADMISSION_REASON, regex('covid', ignore_case = T)),
        age_group,
        name = "n") %>% 
  
  complete(ward, date, age_group, fill = list(n = 0))  %>%
  
  group_by(ward, age_group) %>%
  arrange(date) %>%
  
  mutate(n = zoo::rollsum(n, k = 14, fill = NA)) %>%
  ungroup()%>% 
  
  left_join(case_count, by = c("date" = "date_onset", "age_group")) %>%
  
  mutate(p_adm = n / n_cases)  %>%
  
  drop_na(p_adm)  %>%
  ungroup()
  
ggplot() +
  geom_line(aes(x = date, y = p_adm, colour = ward),
            plot_data) +
  # 
  # ggrepel::geom_label_repel(aes(x = date, y = p_adm, label = ward, colour = ward),
  #                           nudge_x = 10, size = 3,
  #                           plot_data %>% filter(date == max(date)) %>% arrange(desc(p_adm)) %>% slice(1:5)) +
  
  scale_x_date(date_breaks = "months",
               labels = scales::label_date_short(),
               expand = expansion(mult = c(0.05, 0.2))) +
  
  facet_wrap(~age_group, scales = "free_y") +
  # 
  # paletteer::scale_colour_paletteer_d("basetheme::clean",
  #                                     name = "Is 'COVID' in admission reason",
  #                                     labels = c("TRUE" = "Yes", "FALSE" = "No")) +
  
  ggtitle("Probability of hospitalisation given case",
          "Cases over 50 years of age") +
    

  
  theme_minimal() +
  
  theme(legend.position = "bottom")

hosp_by_ward <- hosp_raw %>%
  count(date = as_date(admit_date_dt),
        ward = VISIT_FACILITY_NAME,
        name = "n_admit") %>%
  
  complete(ward,
           date = seq(min(date), max(date), "days"),
           fill = list(n_admit = 0)) %>%
  
  left_join(
    hosp_raw %>%
      count(date = as_date(discharge_date_dt),
            ward = ADMISSION_REASON,
            name = "n_discharge")
  ) %>%
  
  mutate(n_discharge = replace_na(n_discharge, 0)) %>% 
  group_by(ward) %>%
  arrange(date) %>%
  mutate(n_cum = cumsum(n_admit - n_discharge)) %>%
  
  ungroup()



hosp_by_ward %>%
  
  #mutate(ward = reorder(ward, n_cum, sum)) %>%
  arrange(ward) %>%
  
  group_by(date) %>%
  mutate(n_cum_stack = cumsum(n_cum),
         n_cum_stack_bottom = lag(n_cum_stack, default = 0)) %>%
  
  ggplot() +
  geom_ribbon(aes(x = date, ymin = 0, ymax = n_cum, group = ward, fill = factor(ward))) +
  
  facet_wrap(~ward, ncol = 1) +
  
  theme_minimal() +
  
  #ggokabeito::scale_fill_okabe_ito(order = c(6,2,3,5,6,2,3,5,6,2,3,5)) +
  
  theme(legend.position = "none")

  
ggsave(
  "by_subward.pdf",
  height = 150,
  limitsize = FALSE,
  bg = "white",
  width = 8
)


