

get_neuts_change <- function(prediction_dates) {
  read_csv("/home/forecast/source/immune_effect/data/BA2_BA5_interpolation_NSW.csv", show_col_types = FALSE) %>% 
    select(date = Date, proportionBA4BA5 = `Mean BA5`) %>%
    mutate(date = dmy(date)) %>%
    filter(date %in% prediction_dates) %>%
    complete(date = prediction_dates) %>%
    
    mutate(proportionBA4BA5 = if_else(
      is.na(proportionBA4BA5),
      if_else(date <= ymd("2022-06-01"), 0, 1),
      proportionBA4BA5
      
    )) %>%
    
    mutate(neuts_drop_absolute = log(0.44, 10)) %>%
    
    mutate(vaccine_neuts_drop = proportionBA4BA5 * neuts_drop_absolute,
           case_neuts_drop = (1 - proportionBA4BA5) * neuts_drop_absolute)
}

