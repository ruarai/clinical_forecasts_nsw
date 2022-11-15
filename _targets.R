
library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)


tar_option_set(packages = c(
    "tidyverse",
    "lubridate",
    "curvemush"
  ),
  garbage_collection = TRUE
)

source("R/make_case_trajectory.R")
source("R/read_nsw_cases.R")
source("R/get_time_varying_estimates.R")
source("R/get_constant_estimates.R")
source("R/get_constant_estimates_lm.R")

source("R/progression_model.R")
source("R/plot_main_results.R")
source("R/plot_results_by_age.R")

source("R/export_results.R")

source("R/read_hospital_data.R")
source("R/get_public_occupancy.R")
source("R/calculate_occupancy.R")

source("R/plot_time_varying_morbidity.R")

source("R/time_varying_estimates_reversion_scenario.R")


list(
  
  # The date the forecast is run, currently only used for naming the output
  tar_target(date_forecasting, ymd("2022-11-15")),
  
  # The output name of the forecast, used for labeling output files
  tar_target(forecast_name, str_c("fc_", date_forecasting, "_test_lm1")),
  
  tar_target(perform_fitting, FALSE),
  
  # Input files for the forecast. Case forecast is via James Wood, cases_path is for NCIMS, hospital_data_path is for PFP extract
  tar_target(case_forecast_curve_file, "data/cases/Projections_20221114_4wk.csv"),
  tar_target(cases_path, "../email_digester/downloads/case_linelist/20221114 - Case list - Freya Shearer.zip"),
  tar_target(hospital_data_path, "../email_digester/downloads/hospital_linelist/NSW_out_episode_2022_11_15.xlsx"),
  tar_target(ED_data_path, "../email_digester/downloads/ED_linelist/NSW_out_ED_2022_11_15.xlsx"),
  
  # How far into the past does the backcast begin
  tar_target(date_simulation_start, ymd("2021-12-01")),
  
  # Load in the case forecast and format appropriately
  tar_target(
    case_forecast,
    read_csv(case_forecast_curve_file) %>%
      mutate(date_onset = dmy(Var1) - ddays(1), # Assume onset date is ~1 day before reporting, maybe worth revisiting
             n = Inct_1) %>% 
      drop_na(n) %>%
      select(date_onset, n)
  ), 
  
  # Where the output plots (and anything else) will go
  tar_target(
    plot_dir,
    {
      dir <- str_c("results/", forecast_name, "/")
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      return(dir)
    }
  ),
  
  # The mean length-of-stay distribution parameters and some transition probabilities
  tar_target(
    clinical_parameters, 
    {
      read_csv(
        "../los_analysis_competing_risks/results/NSW_2022-11-07_14day/omi_BA5/fits_wide_mean.csv",
        show_col_types = FALSE
      )  %>%
        left_join(
          tibble(
            age_group = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
            scale_onset_to_ward = c(3.41, 3.41, 3.41, 3.41, 3.41, 3.35, 3.35, 3.24, 3.24) * 0.7,
            shape_onset_to_ward = c(1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.9, 1.9, 1.3) * 0.7
          ),
          by = c("age_group")
        )
    }
  ),
  
  # As above, but with bootstrapped uncertainty
  tar_target(
    clinical_parameter_samples, {
      read_csv(
        "../los_analysis_competing_risks/results/NSW_2022-11-07_14day/omi_BA5/fits_wide.csv",
        show_col_types = FALSE
      ) %>%
        left_join(
          tibble(
            age_group = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
            scale_onset_to_ward = c(3.41, 3.41, 3.41, 3.41, 3.41, 3.35, 3.35, 3.24, 3.24) * 0.7,
            shape_onset_to_ward = c(1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.9, 1.9, 1.3) * 0.7
          ),
          by = c("age_group")
        )
               
      
    }
  ),
  
  # Read in the NCIMS data
  tar_target(
    nsw_cases,
    read_nsw_cases(cases_path) %>%
      filter(TEST_TYPE == "PCR"),
    format = "fst_tbl"
  ),
  
  tar_target(
    hospital_data_unfiltered,

    #read_NSW_hospital_data_ED_only(hospital_data_path, ED_data_path)
    
    read_NSW_hospital_data(hospital_data_path)
  ),
  
  # Produce the set of dates used throughout the rest of the forecasting process
  tar_target(
    forecast_dates,
    tibble(
      simulation_start = date_simulation_start,
      forecast_horizon = max(case_forecast$date_onset),
      case_data = ymd(str_extract(cases_path, "\\d{8}")),
      forecast_start = case_data - ddays(3),
      hospital_data = hospital_data_unfiltered$date_data[1]
    )
  ),
  
  tar_target(occupancy_data, calculate_occupancy(hospital_data_unfiltered)),
  tar_target(occupancy_data_aged, calculate_occupancy_aged(hospital_data_unfiltered)),
  
  # Produce a joint case trajectory combining backcast and forecast
  tar_target(
    case_trajectory,
    make_case_trajectory(nsw_cases, case_forecast, forecast_dates, plot_dir)
  ),
  
  # Produce the time-varying estimates of pr_hosp, pr_ICU and pr_age_given_case with bootstrapping
  tar_target(
    time_varying_estimates,
    #get_time_varying_estimates(nsw_cases, hospital_data_unfiltered, clinical_parameters, forecast_dates)
    #get_constant_estimates(nsw_cases, hospital_data_unfiltered, forecast_dates)
    get_constant_estimates_lm(nsw_cases, hospital_data_unfiltered, case_trajectory, forecast_dates)
  ),
  
  
  tar_target(
    morbidity_trajectories_plot,
    plot_morbidity_trajectories(
      time_varying_estimates,
      "NSW",
      forecast_dates %>% mutate(NNDSS = hospital_data),
      14,
      plot_dir
    )
  ),
  
  # Simulate the clinical progression, with fitting if desired
  tar_target(
    sim_results,
    
    run_progression_model(
      time_varying_estimates,
      case_trajectory,
      clinical_parameter_samples,
      occupancy_data,
      forecast_dates,
      do_ABC = perform_fitting
    )
  ),
  
  # Produce the singular plot of ward and ICU occupancy
  tar_target(
    main_results_plots,
    plot_main_results(sim_results, occupancy_data, forecast_dates, plot_dir, forecast_name)
  ),
  
  tar_target(
    aged_plots,
    plot_results_by_age(sim_results, hospital_data_unfiltered, occupancy_data_aged, forecast_dates, plot_dir)
  ),
  
  # Export the quantiles of ward and ICU occupancy as used by Duleepa
  tar_target(
    quant_export,
    
    export_results(sim_results, plot_dir, forecast_name),
    
    format = "file"
  )
)

