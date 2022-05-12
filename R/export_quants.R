

export_results <- function(sim_results, plot_dir, forecast_name) {
  file_out <- paste0(plot_dir, "/", forecast_name, "_result_summaries.csv")
  
  sim_results$results_formatted  %>%
    select(sample, date, group, count) %>%
    filter(group == "ward" | group == "ICU") %>%
    pivot_wider(names_from = group, values_from = count) %>%
    
    group_by(date) %>%
    
    summarise(
      ward_median = median(ward),
      ward_lower90 = quantile(ward, 0.05),
      ward_upper90 = quantile(ward, 0.95),
      ward_lower95 = quantile(ward, 0.025),
      ward_upper95 = quantile(ward, 0.975),
      
      ICU_median = median(ICU),
      ICU_lower90 = quantile(ICU, 0.05),
      ICU_upper90 = quantile(ICU, 0.95),
      ICU_lower95 = quantile(ICU, 0.025),
      ICU_upper95 = quantile(ICU, 0.975),
    ) %>%
    
    write_csv(file_out)
  
  return(file_out)
}