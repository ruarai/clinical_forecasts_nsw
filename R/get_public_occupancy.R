
get_public_occupancy <- function(summary_counts_path) {
  
  data <- read_csv(summary_counts_path)
  
  
  data %>%
    mutate(date = dmy(date)) %>% 
    select(-cases) %>%
    pivot_longer(c(ward, ICU), names_to = "group", values_to = "count") %>%
    drop_na(count)
  
}

