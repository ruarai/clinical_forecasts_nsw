
adjust_time_varying_estimates <- function(time_varying_estimates) {
  
  ## AGE SHIFT
  # shifted_age_estimates <- time_varying_estimates %>%
  #   select(bootstrap, date, age_group, pr_age_given_case) %>%
  #   mutate(date = date + (ymd("2022-06-15") - ymd("2022-03-01")))
  # time_varying_estimates <- time_varying_estimates %>%
  # 
  #   select(-pr_age_given_case) %>%
  # 
  #   left_join(
  #     shifted_age_estimates, by = c("bootstrap", "age_group", "date")
  #   ) %>%
  # 
  #   group_by(bootstrap, age_group) %>%
  # 
  #   fill(pr_age_given_case, .direction = "downup") %>%
  # 
  #   ungroup()
  # 
  
  
  ## VE SHIFT
  # source("../immune_effect/R/age_data.R")
  # ve_table <- read_rds("../immune_effect/data/hosp_multiplier_2022-06-21.rds")
  # 
  # ve_grouped <- ve_table %>%
  #   mutate(age_group = age_bands_to_groups(age_band)) %>%
  # 
  #   group_by(date, ascertainment, variant, age_group) %>%
  #   summarise(m = mean(hospitalisation_multiplier), .groups = "drop")
  # 
  # 
  # 
  # time_varying_estimates <- time_varying_estimates %>%
  # 
  #   expand_grid(
  #     ascertainment = 0.75,
  #     variant = c("Omicron BA2", "Omicron BA4/5")
  #   ) %>%
  # 
  #   left_join(
  #     ve_grouped,
  #     by = c("age_group", "date", "ascertainment", "variant")
  #   ) %>%
  # 
  #   group_by(age_group, bootstrap, ascertainment, variant) %>%
  #   arrange(date) %>%
  # 
  #   fill(m, .direction = "updown") %>%
  #   ungroup() %>%
  # 
  #   mutate(variant = if_else(variant == "Omicron BA2", "BA2", "BA45")) %>%
  #   pivot_wider(names_from = variant, values_from = m, names_prefix = "m_") %>%
  # 
  #   mutate(x = (0.5 * pr_hosp) / m_BA2,
  #          pr_hosp_BA45 = x * m_BA45 + 0.5 * pr_hosp) %>%
  # 
  #   select(bootstrap, age_group, date, pr_age_given_case, pr_hosp = pr_hosp_BA45, pr_ICU, pr_hosp_old = pr_hosp)
  
  ## VE NGM SMOOTH
  adj_hosp <- read_rds("../immune_effect/data/predicted_pr_hosp_BA45.rds")
  time_varying_estimates <- time_varying_estimates %>%
    left_join(adj_hosp %>% rename(pred_pr_age = pr_age_given_case)) %>%

    group_by(bootstrap, age_group) %>%
    arrange(date) %>%
    fill(pred_pr_hosp, pred_pr_age, .direction = "updown")  %>%
    ungroup() %>%

    select(bootstrap, age_group, date, pr_age_given_case = pred_pr_age,
           pr_hosp = pred_pr_hosp, pr_ICU = pr_ICU, pr_hosp_old = pr_hosp, pr_age_old = pr_age_given_case)
  
  

  time_varying_estimates
}
