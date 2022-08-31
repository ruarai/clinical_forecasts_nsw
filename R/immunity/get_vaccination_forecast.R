get_vaccination_forecast <- function(
  quantium_zip_path
) {
  temp_dir <- str_c(tempdir(), "/quantium")
  dir.create(temp_dir, showWarnings = FALSE)
  
  unzip(quantium_zip_path, exdir = temp_dir)
  
  scenarios <- readr::read_csv(str_c(temp_dir, "/dim_scenario.csv"), show_col_types = FALSE)
  
  realistic_scenario_code <- scenarios %>%
    filter(str_detect(booster_uptake, "Realistic")) %>%
    pull(scenario)
  
  
  print(paste0("Scenario: ", realistic_scenario_code))
  
  reff_env$get_quantium_data_dir <- function(...) temp_dir
  
  vaccination_data <- reff_env$read_quantium_vaccination_data()
  
  vaccination_data %>%
    reff_env$aggregate_quantium_vaccination_data_to_state() %>%
    filter(scenario == realistic_scenario_code) 
}
