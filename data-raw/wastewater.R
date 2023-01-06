wastewater = readr::read_csv("http://publichealth.verily.com/api/csv")

usethis::use_data(wastewater, overwrite = TRUE)
