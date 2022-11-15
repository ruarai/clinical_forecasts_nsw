


library(tidyverse)
library(lubridate)

plot_theme <- list(
  theme_bw(),
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 10, hjust = 0),
    text = element_text(family = "Helvetica"),
    panel.grid.minor = element_line(linetype = "dotted", colour = "grey80")
  )
)

ED_linelist <- readxl::read_xlsx(
  "~/source/email_digester/downloads/ED_linelist/NSW_out_ED_2022_11_07.xlsx",
  sheet = 2
)


covid_terms <- c(
  "covid",
  "respiratory",
  "sore throat",
  "fever",
  "flu like",
  "headache",
  "chest"
)

covid_regex <- regex(
  str_c("(", str_c(covid_terms, collapse = "|"), ")", collapse = ""),
  ignore_case = TRUE
)


source("R/age_groups.R")


ED_linelist <- ED_linelist %>%
  mutate(covidlike = str_detect(ed_presenting_problem, covid_regex),
         admitted = str_detect(ed_mode_of_separation_desc, "admitted"),
         age_group = assign_10yr_age_group(age))


ED_linelist %>%
  count(presentation_date, age_group) %>%
  #filter(covidlike) %>% 
  drop_na(age_group) %>% 
  filter(presentation_date >= ymd("2022-07-01")) %>%

  ggplot() +
  
  geom_line(aes(x = presentation_date, y = n)) +
  
  facet_wrap(~age_group) +
  
  plot_theme

ED_counts <- ED_linelist %>%
  filter(covidlike) %>%
  count(presentation_date, age_group)

ED_linelist %>%
  filter(covidlike) %>%
  count(presentation_date) %>% 
  filter(presentation_date >= ymd("2021-07-01")) %>%
  
  ggplot() +
  
  geom_line(aes(x = presentation_date, y = n)) +
  
  plot_theme



hosp_linelist <- readxl::read_xlsx(
  "~/source/email_digester/downloads/hospital_linelist/NSW_out_episode_2022_10_25.xlsx",
  sheet = 1
)



hosp_linelist_ED <- hosp_linelist %>%
  inner_join(
    ED_linelist %>% 
      filter(admitted) %>% 
      select(person_id,
             visit_facility_id,
             presentation_date_dt,
             ed_presenting_problem,
             ed_mode_of_separation,
             ed_mode_of_separation_desc,
             covidlike,
             age_group),
    
    by = c("person_id",
           "VISIT_FACILITY_ID" = "visit_facility_id"#,
           #"admit_date_dt" = "presentation_date_dt"
           )
    ) 



counts <- hosp_linelist_ED %>%
  filter(covidlike) %>%
  mutate(admit_date = as_date(admit_date),
         discharge_date = as_date(discharge_date),
         ncims_calc_onset_dt = as_date(ncims_calc_onset_dt)) %>%
  mutate(count_start = pmax(admit_date, ncims_calc_onset_dt),
         recovery_date = ncims_calc_onset_dt + days(14),
         count_end = pmin(recovery_date, discharge_date)) %>%
  
  filter(recovery_date >= count_start) %>% 
  rowwise() %>%
  mutate(date = list(seq(from = count_start, to = count_end, by =  "day"))) %>%
  unnest(date) %>%
  count(date) %>%
  
  filter(date <= ymd("2022-10-17"))



ggplot() +
  geom_line(aes(x = date, y = n, linetype = "full"),
            counts) +
  
  plot_theme

counts_ICU <- hosp_linelist_ED %>%
  filter(covidlike) %>%
  mutate(admit_date = as_date(first_icu_date),
         discharge_date = as_date(last_icu_date),
         ncims_calc_onset_dt = as_date(ncims_calc_onset_dt)) %>%
  mutate(count_start = pmax(admit_date, ncims_calc_onset_dt),
         count_end = discharge_date) %>%
  
  filter(count_end >= count_start) %>% 
  rowwise() %>%
  mutate(date = list(seq(from = count_start, to = count_end, by =  "day"))) %>%
  unnest(date) %>%
  count(date, age_group) %>%
  
  filter(date <= ymd("2022-10-17"))





ggplot(counts_ICU) +
  geom_line(aes(x = date, y = n, linetype = "full")) +
  
  facet_wrap(~age_group) +
  
  plot_theme



