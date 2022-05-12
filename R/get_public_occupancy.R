
get_public_occupancy <- function(date_forecasting, forecast_dates) {
  
  # This ensures that the occupancy is downloaded whenever date_forecasting changes
  print(paste0("Downloading public occupancy for ", date_forecasting))
  
  # Read in the "true" occupancy as what has been reported via COVID-19 data
  read_csv("https://github.com/M3IT/COVID-19_Data/raw/master/Data/COVID_AU_state.csv",
                                   show_col_types = FALSE) %>%
    select(-state) %>%
    rename(state = state_abbrev) %>%
    
    mutate(ward_cum = hosp_cum - icu_cum) %>%
    select(state, date, ward = ward_cum, ICU = icu_cum) %>%
    
    pivot_longer(cols = -c(state, date),
                 values_to = "count", names_to = "group") %>%
    
    filter(state == "NSW") %>%
    
    filter(date >= forecast_dates$simulation_start)
}