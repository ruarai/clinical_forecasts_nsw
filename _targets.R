
library(targets)

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
source("R/plot_time_varying_estimates.R")

source("R/progression_model.R")
source("R/plot_main_results.R")
source("R/plot_results_by_age.R")

source("R/export_quants.R")

source("R/read_hospital_data.R")

list(
  
  # The date the forecast is run, currently only used for naming the output
  tar_target(date_forecasting, ymd("2022-05-09")),
  
  # The output name of the forecast, used for labelling output files
  tar_target(forecast_name, str_c("fc_", date_forecasting, "_code_test")),
  
  tar_target(perform_fitting, FALSE),
  
  # Input files for the forecast. Case forecast is via James Wood, cases_path is for NCIMS, hospital_data_path is for APDC
  tar_target(case_forecast_curve_file, "~/random_data/JWood/Projections_20220509.csv"),
  tar_target(cases_path, "../email_digester/downloads/20220509 - Case list - Freya Shearer.zip"),
  tar_target(hospital_data_path, "../email_digester/downloads/NSW_out_episode_2022_05_10.xlsx"),
  
  # Path to data for ward and ICU occupancy by age, used for diagnostic plots
  tar_target(occupancy_by_age_ward_path, "~/data_private/NSW_occupancy/Ward_2022-04-26_UNSW.csv"),
  tar_target(occupancy_by_age_ICU_path, "~/data_private/NSW_occupancy/ICU_2022-04-26_UNSW.csv"),
  
  
  # How far into the past does the backcast begin
  tar_target(date_simulation_start, ymd("2021-11-01")),
  
  # Where the output plots (and anything else) will go
  tar_target(
    plot_dir,
    {
      dir <- str_c("results/", forecast_name, "/")
      dir.create(dir, showWarnings = FALSE)
      return(dir)
    }
  ),
  
  # The mean length-of-stay distribution parameters and some transition probabilities
  tar_target(
    clinical_parameters, 
    {
      read_csv(
        "/home/forecast/source/los_analysis_competing_risks/results/NSW_2022-05-03_omi_primary/clinical_parameters_share.csv",
        show_col_types = FALSE
      ) %>%
        # Can't produce onset-to-ward estimates from the NSW data as-is, so use Delta estimates (via JWalk, somehow) (7/02/2022)
        mutate(scale_onset_to_ward = c(3.41, 3.41, 3.41, 3.41, 3.41,
                                       3.35, 3.35, 3.24, 3.24),
               shape_onset_to_ward = c(1.7, 1.7, 1.7, 1.7, 1.7,
                                       1.7, 1.9, 1.9, 1.3))
    }
  ),
  
  # As above, but with bootstrapped uncertainty
  tar_target(
    clinical_parameter_samples, {
      read_csv(
        "../los_analysis_competing_risks/results/NSW_2022-05-03_omi_primary/estimate_samples_share_wide.csv"
      ) %>%
        mutate(scale_onset_to_ward = c(3.41, 3.41, 3.41, 3.41, 3.41,
                                       3.35, 3.35, 3.24, 3.24) %>% rep(times = 1000),
               shape_onset_to_ward = c(1.7, 1.7, 1.7, 1.7, 1.7,
                                       1.7, 1.9, 1.9, 1.3) %>% rep(times = 1000))
      
    }
  ),
  
  # Load in the case forecast and format appropriately
  tar_target(
    case_forecast,
    read_csv(case_forecast_curve_file) %>%
      mutate(date_onset = ymd(Date) - ddays(1), # Assume onset date is ~1 day before reporting, maybe worth revisiting
             n = Cases) %>% 
      drop_na(n) %>%
      select(date_onset, n)
  ), 
  
  # Read in the NCIMS data
  tar_target(
    nsw_cases,
    read_nsw_cases(cases_path),
    format = "fst_tbl"
  ),
  
  # Read in the APDC data (with minimal filtering, but with concatenation of episodes)
  tar_target(
    hospital_data_unfiltered,
    
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
  
  # Produce a joint case trajectory combining backcast and forecast
  tar_target(
    case_trajectory,
    make_case_trajectory(nsw_cases, case_forecast, forecast_dates, plot_dir)
  ),
  
  # Produce the time-varying estimates of pr_hosp, pr_ICU and pr_age_given_case with bootstrapping
  tar_target(
    time_varying_estimates,
    get_time_varying_estimates(nsw_cases, hospital_data_unfiltered, clinical_parameters, forecast_dates)
  ),
  
  # Plot the above estimates
  tar_target(
    time_varying_estimates_plot,
    plot_time_varying_estimates(time_varying_estimates, forecast_dates, plot_dir)
  ),
  
  # Simulate the clinical progression, with fitting if desired
  tar_target(
    sim_results,
    
    run_progression_model(time_varying_estimates, case_trajectory, clinical_parameter_samples, forecast_dates, do_ABC = perform_fitting)
  ),
  
  # Produce the singular plot of ward and ICU occupancy
  tar_target(
    main_results_plots,
    plot_main_results(sim_results, forecast_dates, plot_dir, forecast_name)
  ),
  
  # Produce additional plots of occupancy and transitions by age
  tar_target(
    by_age_plots,
    plot_results_by_age(sim_results, hospital_data_unfiltered, occupancy_by_age_ward_path, occupancy_by_age_ICU_path, forecast_dates, plot_dir)
  ),
  
  # Export the quantiles of ward and ICU occupancy as used by Duleepa
  tar_target(
    quant_export,
    
    export_results(sim_results, plot_dir, forecast_name),
    
    format = "file"
  )
)

