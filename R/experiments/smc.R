
library(targets)
library(tidyverse)
library(lubridate)

time_varying_estimates <- tar_read(time_varying_estimates)
forecast_dates <- tar_read(forecast_dates)

clinical_parameters <- tar_read(clinical_parameters)
case_trajectory <- tar_read(case_trajectory)

clinical_parameter_samples <- tar_read(clinical_parameter_samples)
public_occupancy_data <- tar_read(public_occupancy_data)

source("R/age_groups.R")


date_seq <- seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, "days")

time_varying_estimates_input <- time_varying_estimates %>%
  mutate(ix_age_group = match(age_group, age_groups) - 1,
         ix_sample = bootstrap - 1,
         ix_day = match(date, date_seq) - 1) %>%
  
  group_by(date, bootstrap) %>%
  mutate(pr_age_given_case = pr_age_given_case / sum(pr_age_given_case)) %>%
  ungroup()


case_ensemble_input <- case_trajectory %>%
  mutate(ix_sample = 0,
         ix_day = match(date_onset, date_seq) - 1) %>%
  rename(cases = count)


clinical_parameters_input <- clinical_parameter_samples %>%
  mutate(ix_sample = sample - 1,
         ix_age_group = match(age_group, age_groups) - 1)

n_days <- length(date_seq)

# Produce two vectors (for ward and ICU) of known occupancy to be fit to
occupancy_input <- tibble(
  date = seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, by = 'days')
) %>%
  mutate(do_match = date >= forecast_dates$forecast_start - ddays(7) & date <= forecast_dates$forecast_start) %>%
  
  left_join(
    public_occupancy_data %>%
      select(date, group, count) %>%
      pivot_wider(names_from = group, values_from = count),
    
    by = "date") %>%
  
  mutate(ward_vec = if_else(do_match, ward, -1),
         ICU_vec = if_else(do_match, ICU, -1),
         
         ward_vec = replace_na(ward_vec, -1),
         ICU_vec = replace_na(ICU_vec, -1)) %>%
  
  mutate(ix_day = match(date, date_seq) - 1)

devtools::load_all("../curvemush/")

# Perform the actual simulation and fitting
a <- Sys.time()
results <- curvemush::mush_abc_smc(
  case_ensemble = case_ensemble_input,
  time_varying_estimates = time_varying_estimates_input,
  clinical_parameters = clinical_parameters_input,
  known_occupancy = occupancy_input,
  
  thresholds_vec = seq(1, 0.1, by = -0.05),
  n_particles = 1000,
  n_steps_per_day = 4,
  n_days = n_days,
  n_delay_samples = 512
)

b <- Sys.time()
print(str_c("Simulation ran in ", round(b - a, 2), " ", units(b - a)))

# Do a bunch of work to re-format the simulation output into a nicer form

group_labels <- c("symptomatic_clinical", "ward", "ICU", "discharged", "died")
compartment_labels <- c(
  "symptomatic_clinical", "ward", "discharged_ward", "died_ward", "ICU",
  "discharged_ICU", "died_ICU", "postICU_to_discharge", "postICU_to_death",
  "discharged_postICU", "died_postICU"
)
source("R/progression_model.R")

format_grouped <- . %>%
  mutate(date = forecast_dates$simulation_start + ddays(t_day),
         group = group_labels[compartment_group + 1])

format_ungrouped <- . %>%
  mutate(date = forecast_dates$simulation_start + ddays(t_day),
         compartment = compartment_labels[compartment + 1])



results_count_quants <- results$grouped_results %>%
  select(-c(transitions)) %>%
  pivot_wider(names_from = "sample",
              names_prefix = "sim_",
              values_from = "count") %>%
  make_results_quants() %>%
  format_grouped()

results_ungrouped_count_quants <- results$results %>%
  select(-c(transitions)) %>%
  pivot_wider(names_from = "sample",
              names_prefix = "sim_",
              values_from = "count") %>%
  make_results_quants() %>%
  format_ungrouped()

results_transitions_quants <- results$grouped_results %>%
  select(-c(count)) %>%
  pivot_wider(names_from = "sample",
              names_prefix = "sim_",
              values_from = "transitions") %>%
  make_results_quants() %>%
  format_grouped()

results_ungrouped_transitions_quants <- results$results %>%
  select(-c(count)) %>%
  pivot_wider(names_from = "sample",
              names_prefix = "sim_",
              values_from = "transitions") %>%
  make_results_quants() %>%
  format_ungrouped()


ggplot(results_count_quants %>% filter(group == "ward")) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
              fill = 'blue', alpha = 0.1) +
  
  geom_point(aes(x = date, y = ward_vec), occupancy_input) +
  
  facet_wrap(~group, scales = "free_y")


ggplot(results_count_quants %>% filter(group == "ICU")) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
              fill = 'blue', alpha = 0.1) +
  
  geom_point(aes(x = date, y = ICU_vec), occupancy_input) +
  
  facet_wrap(~group, scales = "free_y")
