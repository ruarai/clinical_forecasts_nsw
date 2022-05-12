
# The set of age-groups used throughout the model
age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

# Reduces numeric ages to the 10-year age groupings used in the model
assign_10yr_age_group <- function(age) {
  age_bottom <- (age %/% 10) * 10
  age_top <- age_bottom + (10 - 1)
  
  case_when(
    age < 0 ~ "invalid",
    age >= 80 ~ "80+",
    TRUE ~ str_c(age_bottom, "-", age_top)
  )
}
