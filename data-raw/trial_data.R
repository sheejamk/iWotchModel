## code to prepare `trial_data` dataset goes here
trial_data <- "data-raw/trial_data.csv"

trial_data  <- read.csv(file = trial_data, stringsAsFactors = F)

# Create .rda object for trial_data in data folder
usethis::use_data(trial_data)
