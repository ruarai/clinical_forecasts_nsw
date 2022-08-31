load("~/source/clinical_forecasting_NSW/debug.rdata")
devtools::load_all("../curvemush/")
results <- curvemush::mush_abc_smc(
  case_ensemble = case_ensemble_input,
  time_varying_estimates = time_varying_estimates_input,
  clinical_parameters = clinical_parameters_input,
  known_occupancy = occupancy_input,
  
  thresholds_vec = 10,#seq(10, 2, by = -0.1),
  n_particles = 1,
  n_steps_per_day = 4,
  n_days = n_days,
  n_delay_samples = 512
)




time_varying_estimates_input %>%
  filter(bootstrap == 100) %>%
  select(-ix_sample) %>%
  left_join(case_ensemble_input, by = "ix_day") %>%
  mutate(cases = pr_age_given_case * cases * pr_hosp) %>%
  group_by(ix_day) %>%
  summarise(cases = sum(cases)) %>%
  
  ggplot() +
  geom_line(aes(x = ix_day, y= cases)) +
  
  geom_point(aes(x = t_day, y = transitions),
             results$results %>%
               as_tibble() %>%
               filter(compartment == 0))
