## code to prepare `adl scoring table` dataset goes here
adl_scoring <- "data-raw/Promis-8a-scoring-table.csv"

adl_scoring  <- read.csv(file = adl_scoring, stringsAsFactors = F)

# Create .rda object for trial_data in data folder
usethis::use_data(adl_scoring)
