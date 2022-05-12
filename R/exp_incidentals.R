

time_varying_estimates <- tar_read(time_varying_estimates)

case_trajectory <- tar_read(case_trajectory)

nsw_pop <- 8.16*10^6

prevalence <- case_trajectory$count * 10



person_days <- c(1194866, 239899, 256441, 580905, 884437, 1148227, 1435410, 1363793, 1232439, 1444831,
                 1597994, 1956304, 2258811, 2639531, 3117958, 2960865, 2752101, 2236220, 1237108, 344122)

age_groups_pd <- c("0-4", "5-9",
                   "10-14",  "15-19",
                   "20-24",  "25-29",
                   "30-34",  "35-39",
                   "40-44",  "45-49",
                   "50-54",  "55-59",
                   "60-64",  "65-69",
                   "70-74",  "75-79",
                   "80-84",  "85-89",
                   "90-94",  "95+")

age_fix_pd <- function(age_group) {
  age_bottom <- str_split(age_group, "-") %>%
    map(function(x) x[1]) %>%
    unlist() %>%
    as.numeric()
  
  new_bottom <- floor(age_bottom / 10) * 10
  new_top <- new_bottom + 9
  
  if_else(is.na(age_bottom), "95+", str_c(new_bottom, "-", new_top))  
}






age_fix <- function(age_group) {
  age_bottom <- str_split(age_group, "-") %>%
    map(function(x) x[1]) %>%
    unlist() %>%
    as.numeric()
  
  new_bottom <- floor(age_bottom / 10) * 10
  new_top <- new_bottom + 9
  
  if_else(is.na(age_bottom), "80+", str_c(new_bottom, "-", new_top))  
}

age_dist <- read_csv("../clinical_forecasting/data/demography/age_distribution_by_state.csv") %>%
  mutate(age_group = age_fix(age_class)) %>%
  group_by(state, age_group) %>%
  summarise(population = sum(population))


person_days_data <- tibble(age_group = age_groups_pd, person_days = person_days) %>%
  mutate(age_group = age_fix_pd(age_group),
         age_group = if_else(age_group %in% c("80-89", "90-99", "95+"), "80+", age_group)) %>%
  group_by(age_group) %>%
  summarise(person_days = sum(person_days),
            daily_persons = person_days / 365) %>%
  
  left_join(age_dist %>% filter(state == "NSW")) %>%
  
  mutate(daily_prop_admitted = daily_persons / population) %>%
  
  select(age_group, daily_prop_admitted, population)




prop_prevalence <- prevalence / nsw_pop

plot(prop_prevalence)

model_data <- time_varying_estimates %>%
  select(-pr_ICU) %>% 
  filter(date <= ymd("2022-04-10")) %>%
  left_join(case_trajectory, by = c("date" = "date_onset")) %>%
  left_join(person_days_data) %>%
  
  
  mutate(count = count * pr_age_given_case,
         prevalence = count * 14,
         prevalence = prevalence / population,
         
         predicted_incidentals = prevalence * daily_prop_admitted * 0.4,
         predicted_incidentals = predicted_incidentals * population)

# pr_hosp = pr_age_given_case * true_severity + prevalence * incidence_conversion


ggplot(model_data %>% filter(bootstrap == 1)) +
  geom_line(aes(x = date, y = predicted_incidentals, color = age_group))



ward_occupancy <- read_csv("~/data_private/NSW_occupancy/Ward_2022-04-26_UNSW.csv") %>%
  select(date = DATE, date_snapshot = SNAPSHOT_DATE,
         
         age_group = AGE_GROUP_10YR, count_PCR = PCR_Ward, count_RAT = RAT_Ward) %>%
  pivot_longer(cols = c(count_PCR, count_RAT),
               names_prefix = "count_",
               names_to = "type", values_to = "count")


plot_data <- ward_occupancy %>%
  mutate(age_group = str_remove(age_group, " years"),
         age_group = if_else(age_group == "80-89" | age_group == "90+", "80+", age_group)) %>%
  filter(date >= model_data$date %>% min()) %>%
  group_by(date, age_group) %>%
  summarise(occupancy = sum(count)) %>%
  left_join(model_data) %>%
  
  mutate(predicted_true = occupancy - predicted_incidentals)

ggplot(plot_data %>% filter(bootstrap == 50)) +
  geom_line(aes(x = date, y = predicted_true, color = age_group)) +
  
  ggokabeito::scale_color_okabe_ito() +
  
  theme_minimal()
