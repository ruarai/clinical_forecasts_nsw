


assign_age_band <- function(age) {
  case_when(
    age <= 4 ~ "0-4",
    age <= 11 ~ "5-11",
    age <= 15 ~ "12-15",
    age <= 19 ~ "16-19",
    age <= 24 ~ "20-24",
    age <= 29 ~ "25-29",
    age <= 34 ~ "30-34",
    age <= 39 ~ "35-39",
    age <= 44 ~ "40-44",
    age <= 49 ~ "45-49",
    age <= 54 ~ "50-54",
    age <= 59 ~ "55-59",
    age <= 64 ~ "60-64",
    age <= 69 ~ "65-69",
    age <= 74 ~ "70-74",
    age <= 79 ~ "75-79",
    age >= 80 ~ "80+"
  )
}


age_group_to_bands <- function(age_group) {
  case_when(
    age_group == "0-9"   ~ list(c("0-4", "5-11"))   ,
    age_group == "10-19" ~ list(c("12-15", "16-19")),
    age_group == "20-29" ~ list(c("20-24", "25-29")),
    age_group == "30-39" ~ list(c("30-34", "35-39")),
    age_group == "40-49" ~ list(c("40-44", "45-49")),
    age_group == "50-59" ~ list(c("50-54", "55-59")),
    age_group == "60-69" ~ list(c("60-64", "65-69")),
    age_group == "70-79" ~ list(c("70-74", "75-79")),
    age_group == "80+"   ~ list(c("80+"))           )
}




age_bands_to_groups <- function(age_group_vacc) {
  case_when(
    age_group_vacc %in% c("0-4", "5-11") ~ "0-9",
    age_group_vacc %in% c("12-15", "16-19") ~ "10-19",
    age_group_vacc %in% c("20-24", "25-29") ~ "20-29",
    age_group_vacc %in% c("30-34", "35-39") ~ "30-39",
    age_group_vacc %in% c("40-44", "45-49") ~ "40-49",
    age_group_vacc %in% c("50-54", "55-59") ~ "50-59",
    age_group_vacc %in% c("60-64", "65-69") ~ "60-69",
    age_group_vacc %in% c("70-74", "75-79") ~ "70-79",
    age_group_vacc %in% c("80+") ~ "80+")
}



age_band_order <- c("0-4", "5-11", "12-15", "16-19", "20-24", "25-29", "30-34", 
                    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
                    "70-74", "75-79", "80+")

age_group_order <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", 
                     "70-79", "80+")
