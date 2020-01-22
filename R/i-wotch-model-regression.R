param_file = system.file("extdata","table_param.csv", package = "iWotchModel")

#utility_IT

iwotch_trial_data <- load_trial_data()
arm_details <- get_trial_arm_details(trial_data)
time_point_detauls <- get_timepoint_details(trial_data)
eq5d_details <- get_eq5d_details(trial_data)
trial_data <- value_eq5d5L_IPD(trial_data,NA)
arm_details
help("get_parameter_estimated_regression")
colnames(trial_data)
get_parameter_estimated_regression("EQ5D5LIndex",trial_data,"linear regression", arm_details$name,NA,NA)
