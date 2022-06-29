

get_ascertainment_timeseries <- function(pred_dates) {
  
  tibble(
    date = seq(ymd("2020-01-01"), ymd("2024-01-01"), by = "days")
  ) %>%
    left_join(
      tribble(
        ~date, ~ascertainment,
        ymd("2021-12-01"), 0.75,
        ymd("2021-12-12"), 0.33,
        ymd("2022-01-22"), 0.33,
        ymd("2022-03-01"), 0.75,
      ),
      by = "date"
    ) %>%
    
    mutate(ascertainment = zoo::na.approx(ascertainment, rule = 2)) %>%
    
    filter(date %in% pred_dates)
}

