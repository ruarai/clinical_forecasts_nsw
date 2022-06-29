
get_params <- function(scenario = "estimate") {
  params_wide <- reff_env$get_omicron_params_wide()
  
  neut_params_wide <- params_wide %>%
    select(
      omicron_scenario,
      log10_mean_neut_AZ_dose_1,
      log10_mean_neut_AZ_dose_2,
      log10_mean_neut_Pfizer_dose_1,
      log10_mean_neut_Pfizer_dose_2,
      log10_mean_neut_mRNA_booster,
      log10_mean_neut_infection,
      neut_decay
    ) %>%
    filter(omicron_scenario == scenario)
  
  ve_params_wide <- params_wide %>%
    select(
      omicron_scenario,
      starts_with("c50"),
      log_k,
      sd_log10_neut_titres,
      omicron_log10_neut_fold
    ) %>%
    filter(omicron_scenario == scenario)
  
  list("ve" = ve_params_wide, "neut" = neut_params_wide)
}