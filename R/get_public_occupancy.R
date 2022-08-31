
get_public_occupancy <- function(date_forecasting, forecast_dates) {
  
  # This ensures that the occupancy is downloaded whenever date_forecasting changes
  print(paste0("Downloading public occupancy for ", date_forecasting))
  
  
  # If covidlive dies, try https://covidbaseau.com/historical/?title=Hospitalisations%20NSW&return=https://covidbaseau.com/nsw/
  
  
  scrape <- function(states = NULL) {
    require(rvest)
    
    if (is.null(states)) {
      states <- c("act","nsw","nt","qld","sa","tas","vic","wa")
    }
    #to lower incase input are upper
    states <- tolower(states)
    
    urls <- paste0("https://covidlive.com.au/report/daily-hospitalised/",states)
    
    map_dfr(1:length(states), function(state) {
      url <- urls[state]
      state_ll <- url %>%
        read_html() %>%
        html_nodes(
          "table"
        ) %>%
        .[[2]] %>%
        html_table(
          fill = TRUE
        ) %>%
        mutate(
          date = as.Date(DATE, format = "%d %B %y"),
          hosp = as.numeric(gsub(",","",HOSP)),
          hosp = ifelse(is.na(hosp),0,hosp),
          hosp = ifelse(hosp < 0,0,hosp),
          icu = as.numeric(gsub(",","",ICU)),
          icu = ifelse(is.na(icu),0,icu),
          icu = ifelse(icu < 0,0,icu),
          state = toupper(states[state])
        ) %>%
        arrange(
          date
        ) %>%
        mutate(ward = hosp - icu,
               date = date - ddays(1)) %>%
        select(state, date, ward, ICU = icu) %>%
        
        pivot_longer(c("ward", "ICU"),
                     names_to = "group",
                     values_to = "count")
    })
  } 
  
  scrape("NSW") %>%
    filter(date >= forecast_dates$simulation_start)
}

