microcosting_pat <- function(datafile,
                                 name_med, dose_med, dose_unit,
                                 no_taken, freq_taken, basis_time,
                                 unit_cost_file, unit_cost_column,
                                 cost_calculated_in, strength_expressed_in,
                                 list_period_timepoint,
                                 list_of_code_names = NULL,
                                 list_of_code_freq = NULL,
                                 list_of_code_dose_unit = NULL,
                                 equiv_dose = NULL){

  ind_part_data = packDAMipd::load_trial_data(datafile)
  unit_cost_data = packDAMipd::load_trial_data(unit_cost_file)
  if (is.null(list_period_timepoint)) {
    stop("Error - time period for the use of medication at each time point is required")
  }
  if (is.null(equiv_dose)) {
    equiv_dose_div = 1
  }else{
    equiv_dose_div = ind_part_data[[equiv_dose]]
  }
  # check colmns exist in individual data
  info_list = c(name_med, dose_med, dose_unit,no_taken, freq_taken)
  checks = sapply(info_list, IPDFileCheck::check_column_exists, ind_part_data)
  if (sum(checks) != 0) {
    stop("Any one of the required columns can not be found in data")
  }
  # check colmns exist in unit cost data
  if (is.null(unit_cost_data) | is.null(unit_cost_column) | is.null(cost_calculated_in)) {
    stop("Error - Provide the unit costs data")
  }
  if (IPDFileCheck::check_column_exists(unit_cost_column,unit_cost_data) != 0)
    stop("Error - unit cost column should exist in the unit cost data")

  # get colnames for name, form, dosage and unit
  name_pattern = c("name","drug", "medication", "med")
  bool_res = unlist(lapply(name_pattern, IPDFileCheck::check_colno_pattern_colname,colnames(unit_cost_data)))
  if (any(bool_res))
    name_ind = which( bool_res == TRUE)
  else
    stop("Error- Name of the medication is not found. Please use name, drug, medication, or med to indicate the medication")
  name_col_no = IPDFileCheck::get_colno_pattern_colname(name_pattern[name_ind],colnames(unit_cost_data))

  form_pattern = c("form","drug form", "patch/tablet/liquid", "type")
  bool_res = unlist(lapply(form_pattern, IPDFileCheck::check_colno_pattern_colname,colnames(unit_cost_data)))
  if (any(bool_res))
    form_ind = which( bool_res == TRUE)
  else
    stop("Error- Form of the medication is not found. Please use form, drug form, patch/tablet/liquid, or type to indicate
         the type of medication")
  form_col_no = IPDFileCheck::get_colno_pattern_colname(form_pattern[form_ind],colnames(unit_cost_data))

  dosage_col_no = IPDFileCheck::get_columnno_fornames(unit_cost_data, strength_expressed_in)

  unit_col_no = IPDFileCheck::get_columnno_fornames(unit_cost_data, cost_calculated_in )

  # if the codes are being used for name, dosage, frequency an time period
  if (!is.null(list_of_code_names)) {
    name_and_code <- stats::setNames(as.list(list_of_code_names[[1]]),list_of_code_names[[2]])
    ipd_codes = ind_part_data[[name_med]]
    name_from_code = name_and_code[ipd_codes]
  }else{
    name_from_code = ind_part_data[[name_med]]
  }
  if (!is.null(list_of_code_freq)) {
    freq_code <- stats::setNames(as.list(list_of_code_freq[[1]]),list_of_code_freq[[2]])
    ipd_codes = ind_part_data[[freq_taken]]
    freq_desc_from_code = freq_code[ipd_codes]
    freq_given_basis = unlist(lapply(freq_desc_from_code,packDAMipd::convert_freq_diff_basis, basis_time))
  }else{
    freq_given_basis = unlist(lapply(ind_part_data[[freq_taken]],packDAMipd::convert_freq_diff_basis, basis_time))
  }
  if (!is.null(list_of_code_dose_unit)) {
    unit_and_code <- stats::setNames(as.list(list_of_code_dose_unit[[1]]),list_of_code_dose_unit[[2]])
    ipd_codes = ind_part_data[[dose_unit]]
    unit_from_code = unit_and_code[ipd_codes]
  }else{
    unit_from_code = ind_part_data[[dose_unit]]
  }
  timepoint_details = packDAMipd::get_timepoint_details(ind_part_data)
  timepoint_codes = ind_part_data[[timepoint_details$name]]
  period_code <- stats::setNames(as.list(list_period_timepoint[[1]]),list_period_timepoint[[2]])
  period_desc_from_code = period_code[timepoint_codes]

  list_total_med_basis <- list()
  list_total_cost_basis <- list()
  list_total_cost_basis_equiv_dose <- list()
  list_total_cost_timeperiod <- list()
  list_total_cost_timeperiod_equiv_dose <- list()
  for (i in 1:nrow(ind_part_data)) {
    name_medication = name_from_code[i]
    dose_medication = ind_part_data[[dose_med]][i]
    if (!is.null(dose_medication)  & !is.na(dose_medication)) {
      how_many_taken = ind_part_data[[no_taken]][i]
      freq_mutliplier = freq_given_basis[i]
      this_unit = unit_from_code[i]
      subset1 = unit_cost_data[toupper(unit_cost_data[[name_col_no]]) == toupper(name_medication),]
      subset2 = subset1[subset1[form_col_no] == "Patch" | subset1[form_col_no] == "Patches",]
      unit_used_costing = unique(subset2[[unit_col_no]])
      if (length(unit_used_costing) != 1)
        stop("unit used for costing tablets is not unique !!!")
      total_med_basis = how_many_taken * freq_mutliplier * dose_medication
      unit_multiplier = packDAMipd::convert_wtpertimediff_basis(this_unit, unit_used_costing)
      if (unit_multiplier != 1)
        stop("The unit used is not same as that used in calculating costs")

      if (any(subset2[[dosage_col_no]] == dose_medication)) {
        unit_cost_med_prep = subset2[subset2[[dosage_col_no]] == dose_medication &
                                       subset2[[unit_col_no]] ==  unit_used_costing,][[unit_cost_column]]

      }else{
        stop("The used dosage is not in costing table")
      }

      total_cost_basis =  total_med_basis * unit_cost_med_prep * unit_multiplier
      total_cost_basis_equiv_dose =  total_cost_basis / equiv_dose_div[i]
      period_given_basis = packDAMipd::convert_to_given_timeperiod(period_desc_from_code[i], basis_time)
      total_cost_timeperiod =  total_cost_basis * period_given_basis
      total_cost_timeperiod_equiv_dose =  total_cost_timeperiod / equiv_dose_div[i]
    }else{
      total_med_basis = NA
      total_cost_basis = NA
      total_cost_basis_equiv_dose = NA
      total_cost_timeperiod = NA
      total_cost_timeperiod_equiv_dose = NA
    }
    list_total_med_basis <- append(list_total_med_basis,total_med_basis)
    list_total_cost_basis <- append(list_total_cost_basis,total_cost_basis)
    list_total_cost_basis_equiv_dose <- append(list_total_cost_basis_equiv_dose, total_cost_basis_equiv_dose)
    list_total_cost_timeperiod <- append(list_total_cost_timeperiod, total_cost_timeperiod)
    list_total_cost_timeperiod_equiv_dose <- append(list_total_cost_timeperiod_equiv_dose,total_cost_timeperiod_equiv_dose)
  }
  ind_part_data[["tot_basis_patches"]] = unlist(list_total_med_basis)
  ind_part_data[["totcost_patches_basis"]] = unlist(list_total_cost_basis)
  ind_part_data[["totcost_patches_basis_equiv_dose"]] = unlist(list_total_cost_basis_equiv_dose)
  ind_part_data[["totcost_timeperiod_patches"]] = unlist(list_total_cost_timeperiod)
  ind_part_data[["totcost_patches_timeperiod_equiv_dose"]] = unlist(list_total_cost_timeperiod_equiv_dose)

  return(ind_part_data)
}
