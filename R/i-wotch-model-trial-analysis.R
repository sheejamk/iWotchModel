library(packDAMipd)

##  Load the required data files - IPD trial data, unit cost data for medication
i_wotch_data_file = system.file("extdata","trial_data.csv", package = "iWotchModel")
med_costs_file = system.file("extdata", "costs_per_dosage.csv", package = "iWotchModel")
resource_use_costs_file = system.file("extdata", "costs_resource_use.csv", package = "iWotchModel")

iwotch_trial_data <- load_trial_data(i_wotch_data_file)
med_costs <- load_trial_data(med_costs_file)
unit_cost_data = load_trial_data(resource_use_costs_file)

##  details of arms and timepoints
arm_details <- get_trial_arm_details(iwotch_trial_data)
time_point_details <- get_timepoint_details(iwotch_trial_data)

##  details of arms and tiempoints as they are coded
list_of_code_timepoint  = list(c("Baseline", "4 months", "8 months", "12 months"), c(1,2,3,4))
time_code <- stats::setNames(as.list(list_of_code_timepoint[[1]]),
                             list_of_code_timepoint[[2]])

##  details of outcome
adl_details = get_outcome_details(iwotch_trial_data, "ADL","tpi_Pain", multiple = TRUE)
shows_details = get_outcome_details(iwotch_trial_data, "ShOWS","qsy_Symp", multiple = TRUE)
eq5d_details <- get_eq5d_details(iwotch_trial_data)

##  For total data set calculate outcomes
iwotch_trial_data_ADL = value_ADL_scores_IPD(iwotch_trial_data,"tpi_Pain",adl_scoring = adl_scoring, adl_nrcode = NA)
iwotch_trial_data_Shows = value_Shows_IPD(iwotch_trial_data_ADL,"qsy_Symp", shows_nrcode = NA)
iwotch_trial_data_eq5d = value_eq5d5L_IPD(iwotch_trial_data_Shows, eq5d_nrcode = NA)
iwotch_trial_data_eq5dmap = map_eq5d5Lto3L_VanHout(iwotch_trial_data_eq5d, eq5d_nrcode = NA)


##  For total data set analyse cost a, medication costs
patches = microcosting_patches(iwotch_trial_data_eq5dmap,"Drug", "patch_strength", "patch_dose_unit","patch_no_taken",
                               "patch_frequency",  "day", med_costs, "UnitCost", "StrengthUnit","Strength",
                               list(c("4 weeks", "1 week"), c(1,2)), NULL,
                               list(c("Once a day", "twice a day", "once weekly"), c(1,2,3)),
                               list(c("mcg/hr", "mg/day", "mg/hr"), c(1,2,3)), "patch_equiv_dose")

patches_tablets = microcosting_tablets(patches, "Drug", "tab_dosage", "tab_dosage_unit","tab_no_taken",
                               "tab_frequency", "day", med_costs, "UnitCost",
                               "StrengthUnit",  "Strength",list(c("4 weeks", "1 week"), c(1,2)),
                               NULL,
                               list(c("Once a day", "bd", "once weekly"), c(1,2,3)),
                               list(c("mcg","mg", "gm"), c(1,2,3)), "tab_equiv_dose")
iwotch_trial_data_allmed = microcosting_liquids(patches_tablets, "Drug", "liq_dosage", "liquid_dose_unit", "liquid_bottle_size",
                               "liquid_bottle_remain_time",  NULL, med_costs, "UnitCost",
                               "StrengthUnit", "Strength",list(c("4 weeks", "1 week"), c(1,2)),
                               preparation = NULL, NULL, NULL, NULL, "liquid_equiv_dose")


## total medication costs
cols = c("totcost_liquids_timeperiod_equiv_dose", "totcost_patches_timeperiod_equiv_dose",
         "totcost_tablets_timeperiod_equiv_dose")

medication_sum <- 0
for(i in 1:length(cols)){
  this_col <- iwotch_trial_data_allmed[[cols[i]]]
  this_col[is.na(this_col)] <- 0
  medication_sum <- medication_sum + this_col
}
iwotch_trial_data_allmed[["all_medications_timeperiod"]]<- medication_sum

## Costs for resource use
inpatient_admission = costing_resource_use(iwotch_trial_data_allmed, "tru_ResAdmittedInpatient","rip_StayLength",
                           "rip_NHSHospital","day", unit_cost_data, "Inpatient hospital admissions","UnitCost",
                           "UnitUsed", list(c("Yes", "No"), c(1,2)), list(c("Yes", "No"), c(1,2)))




## Different treatment arms
trial_arm_columnname = arm_details$name
i_wotch_control = iwotch_trial_data_allmed[iwotch_trial_data_allmed[trial_arm_columnname] == "N",]
i_wotch_intervention = iwotch_trial_data_allmed[iwotch_trial_data_allmed[trial_arm_columnname] == "Y",]

## Summarise cost and outcome into tables for diferent treatment arms
no_timepoints = length(time_point_details$codes)
health_benefits = list()
med_costs = list()
this_benefits = list()
this_med_costs = list()
for (i in 1:no_timepoints) {
  codes = sort(time_point_details$codes)
  time_desc_from_code = time_code[codes[i]]

  intervention_this_tp = i_wotch_intervention[i_wotch_intervention[[time_point_details$name]]
                                                         == codes[i],]

  average_ADL_intervention_tp = IPDFileCheck::descriptive_stats_col(intervention_this_tp, "ADLTscore", NA)[,2]
  average_shows_intervention_tp = IPDFileCheck::descriptive_stats_col(intervention_this_tp, "ShOWSscore", NA)[,2]
  average_eq5d5l_intervention_tp = IPDFileCheck::descriptive_stats_col(intervention_this_tp, "EQ5D5LIndex", NA)[,2]
  average_eq5d3l_intervention_tp = IPDFileCheck::descriptive_stats_col(intervention_this_tp, "EQ5D3L_from5L", NA)[,2]
  average_allmedcosts_intervention_tp = IPDFileCheck::descriptive_stats_col(intervention_this_tp, "all_medications_timeperiod", NA)[,2]
  average_allmedcosts_intervention_tp = IPDFileCheck::descriptive_stats_col(intervention_this_tp, "all_medications_timeperiod", NA)[,2]
  total_allmedcosts_intervention_tp = IPDFileCheck::descriptive_stats_col(intervention_this_tp, "all_medications_timeperiod", NA)[,1]

  control_this_tp = i_wotch_control[i_wotch_control[[time_point_details$name]]
                                                == time_point_details$codes[i],]
  average_ADL_control_tp = IPDFileCheck::descriptive_stats_col(control_this_tp, "ADLTscore", NA)[,2]
  average_shows_control_tp = IPDFileCheck::descriptive_stats_col(control_this_tp, "ShOWSscore", NA)[,2]
  average_eq5d5l_control_tp = IPDFileCheck::descriptive_stats_col(control_this_tp, "EQ5D5LIndex", NA)[,2]
  average_eq5d3l_control_tp = IPDFileCheck::descriptive_stats_col(control_this_tp, "EQ5D3L_from5L", NA)[,2]
  average_allmedcosts_control_tp = IPDFileCheck::descriptive_stats_col(control_this_tp, "all_medications_timeperiod", NA)[,2]
  total_allmedcosts_control_tp = IPDFileCheck::descriptive_stats_col(control_this_tp, "all_medications_timeperiod", NA)[,1]


  this_benefits = data.frame(rbind(average_ADL_control_tp, average_shows_control_tp,
                                   average_eq5d5l_control_tp,average_eq5d3l_control_tp,
                                   average_ADL_intervention_tp,average_shows_intervention_tp,
                                   average_eq5d5l_intervention_tp,average_eq5d3l_intervention_tp))
  this_benefits <- t(this_benefits)
  colnames(this_benefits) = c("ADL_control (Mean)","ShoWS_control (Mean)","EQ5D5L_control (Mean)","EQ5D3L_control (Mean)",
                              "ADL_intervention (Mean)","ShoWS_intervention (Mean)","EQ5D5L_intervention (Mean)","EQ5D3L_intervention (Mean)")
  rownames(this_benefits) <-  time_desc_from_code

  health_benefits = rbind(this_benefits,health_benefits)

  this_med_costs = data.frame(rbind(average_allmedcosts_intervention_tp, average_allmedcosts_control_tp,
                                    total_allmedcosts_intervention_tp, total_allmedcosts_control_tp))

  this_med_costs <- t(this_med_costs)
  colnames(this_med_costs) = c("Medcosts_intervention (Mean per patient)","Medcosts_control (Mean per patient)",
                               "Medcosts_intervention (Total)","Medcosts_control (Total)")
  rownames(this_med_costs) <-  time_desc_from_code
  med_costs = rbind(med_costs,this_med_costs)

}


