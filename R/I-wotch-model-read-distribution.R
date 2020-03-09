# Trying with some parameters sampled from predefined distributions - all assumptions
library(packDAMipd)
## initiliase the health states
LTOT <- health_state("LTOT","cost_LTOT","util_LTOT")
IT <- health_state("IT","cost_IT","util_IT")
PT <- health_state("PT","cost_PT","util_PT")
SOLMP <- health_state("SOLMP","cost_SOLMP","util_SOLMP")
DH <- health_state("DH",0,0)

## allowed transitions between health states the health states
trans_intervention <- rbind(c(1, 2, 3, NA, 4),
                            c(5, 6, NA, 7, 8),
                            c(9, NA, 10, 11, 12),
                            c(13, NA, NA, 14, 15),
                            c(NA, NA, NA, NA, 16))
trans_control <-  rbind(c(1, 2, NA, 3),
                        c(4, 5, 6, 7),
                        c(8, NA, 9, 10),
                        c(NA, NA, NA, 11))

colnames(trans_intervention) <- rownames(trans_intervention) <- c("LTOT","IT","PT","SOLMP","DH")
colnames(trans_control) <- rownames(trans_control) <- c("LTOT","PT","SOLMP","DH")

## parameterise the transition probbailities in the transition matrix
trans_matrix_intervention <- transition_matrix(5, trans_intervention,
                                               c("tp_LTOT_LTOT","tp_LTOT_IT", "tp_LTOT_PT",
                                                 "tp_LTOT_DH","tp_IT_LTOT","tp_IT_IT", "tp_IT_SOLMP",
                                                 "tp_IT_DH","tp_PT_LTOT", "tp_PT_PT", "tp_PT_SOLMP",
                                                 "tp_PT_DH","tp_SOLMP_LTOT", "tp_SOLMP_SOLMP",
                                                 "tp_SOLMP_DH","tp_DH_DH"), colnames(trans_intervention))
trans_matrix_control <- transition_matrix(4, trans_control,
                                          c("tp_LTOT_LTOT", "tp_LTOT_PT",
                                            "tp_LTOT_DH","tp_PT_LTOT", "tp_PT_PT", "tp_PT_SOLMP",
                                            "tp_PT_DH","tp_SOLMP_LTOT", "tp_SOLMP_SOLMP",
                                            "tp_SOLMP_DH","tp_DH_DH"), colnames(trans_control))

param_file = system.file("extdata","table_param.csv", package = "iWotchModel")
read.csv(param_file)
param_interv <- define_parameters(
  tp_LTOT_IT = get_parameter_def_distribution("tp_LTOT_IT", param_file, "Strategy","Intervention"),
  tp_LTOT_PT = get_parameter_def_distribution("tp_LTOT_PT", param_file, "Strategy","Intervention"),
  tp_LTOT_DH = get_parameter_def_distribution("tp_LTOT_DH", param_file, "Strategy","Intervention"),
  tp_LTOT_LTOT = "1 - (tp_LTOT_IT + tp_LTOT_PT + tp_LTOT_DH)",
  tp_IT_LTOT = get_parameter_def_distribution("tp_IT_LTOT", param_file, "Strategy","Intervention"),
  tp_IT_SOLMP = get_parameter_def_distribution("tp_IT_SOLMP", param_file, "Strategy","Intervention"),
  tp_IT_DH = get_parameter_def_distribution("tp_IT_DH", param_file, "Strategy","Intervention"),
  tp_PT_LTOT = get_parameter_def_distribution("tp_PT_LTOT", param_file, "Strategy","Intervention"),
  tp_IT_IT =   "1 - (tp_IT_SOLMP + tp_IT_DH + tp_IT_LTOT)",
  tp_PT_SOLMP = get_parameter_def_distribution("tp_PT_SOLMP", param_file, "Strategy","Intervention"),
  tp_PT_DH = get_parameter_def_distribution("tp_PT_DH", param_file, "Strategy","Intervention"),
  tp_PT_PT = "1 - (tp_PT_SOLMP + tp_PT_DH + tp_PT_LTOT)",
  tp_SOLMP_LTOT = get_parameter_def_distribution("tp_SOLMP_LTOT", param_file, "Strategy","Intervention"),
  tp_SOLMP_DH = get_parameter_def_distribution("tp_SOLMP_DH", param_file, "Strategy","Intervention"),
  tp_SOLMP_SOLMP =  "1 - (tp_SOLMP_LTOT + tp_SOLMP_DH)",

  tp_DH_DH = 1,
  cost_IT = 2000,
  cost_PT = 1000,
  cost_LTOT = 1500,
  cost_SOLMP = 800,
  util_IT = 0.6,
  util_PT = 0.5,
  util_LTOT = 0.4,
  util_SOLMP = 0.8
)
# #
param_control <- define_parameters(
  tp_LTOT_PT = get_parameter_def_distribution("tp_LTOT_PT", param_file, "Strategy","Control"),
  tp_LTOT_DH = get_parameter_def_distribution("tp_LTOT_DH", param_file, "Strategy","Control"),
  tp_LTOT_LTOT = "1 - (tp_LTOT_PT + tp_LTOT_DH)",
  tp_PT_LTOT = get_parameter_def_distribution("tp_PT_LTOT", param_file, "Strategy","Control"),
  tp_PT_SOLMP = get_parameter_def_distribution("tp_PT_SOLMP", param_file, "Strategy","Control"),
  tp_PT_DH = get_parameter_def_distribution("tp_PT_DH", param_file, "Strategy","Control"),
  tp_PT_PT = "1 - (tp_PT_SOLMP + tp_PT_DH + tp_PT_LTOT)",
  tp_SOLMP_LTOT = get_parameter_def_distribution("tp_SOLMP_LTOT", param_file, "Strategy","Control"),
  tp_SOLMP_DH = get_parameter_def_distribution("tp_SOLMP_DH", param_file, "Strategy","Control"),
  tp_SOLMP_SOLMP =  "1 - (tp_SOLMP_LTOT + tp_SOLMP_DH)",
  tp_DH_DH = 1,
  cost_IT = 0,
  cost_PT = 1000,
  cost_LTOT = 1500,
  cost_SOLMP = 800,
  util_IT = 0,
  util_PT = 0.5,
  util_LTOT = 0.4,
  util_SOLMP = 0.8
)

# ## intervention markov
health_states <- combine_state(LTOT,IT,PT,SOLMP,DH)
interv_strategy <- strategy(trans_matrix_intervention, health_states, "intervention")
interv_markov <- markov_model(interv_strategy, 4, c(1,0,0,0,0), c(0,0,0,0,0), c(0,0,0,0,0),
                               discount=c(0,0),param_interv)
 plot_model(interv_markov)
# ## control markov
health_states <- combine_state(LTOT,PT,SOLMP,DH)
control_strategy <- strategy(trans_matrix_control, health_states, "control")
control_markov <-markov_model(control_strategy, 4, c(1,0,0,0), c(0,0,0,0), c(0,0,0,0), discount=c(0,0),param_control)
plot_model(control_markov)

list_markov <- combine_markov(control_markov,interv_markov)
result<-calculate_icer_nmb(list_markov = list_markov, threshold = 2000, comparator = "control")
plotceac<-plot_ceac(list_markov,threshold_values = seq(1000,20000,1000),comparator = "control")
plotceac
#
#
#
