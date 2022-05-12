
read_nsw_cases <- function(path) {
  nsw_cases_spec <- cols(
    CASE_ID = col_double(),
    CLASSIFICATION = col_character(),
    EARLIEST_CONFIRMED_OR_PROBABLE = col_date(format = ""),
    SYMPTOM_ONSET_DATE = col_date(format = ""),
    CALCULATED_ONSET_DATE = col_date(format = ""),
    AGE_AT_EVENT_YEARS = col_double(),
    GENDER = col_character(),
    PLACE_ACQUISITION = col_character(),
    LIKELY_SOURCE_OF_INFECTION = col_character(),
    LIKELY_SOURCE_OF_INFECTION_LOCAL = col_character(),
    SYMPTOMS = col_character(),
    DATE_ISOLATION_BEGAN = col_date(format = ""),
    SETTING_OF_TRANSMISSION_LOCATION = col_character(),
    SETTING_OF_TRANSMISSION_DATE = col_date(format = ""),
    SETTING_OF_TRANSMISSION_WORK_RELATED = col_character(),
    PATIENT_INTERVIEWED = col_character(),
    INTERVIEWED_DATE = col_date(format = ""),
    S_gene_result_date = col_date(format = ""),
    Omicron_Category = col_character(),
    TEST_TYPE = col_character()
  )
  
  vroom::vroom(path, col_types = nsw_cases_spec)
  
}