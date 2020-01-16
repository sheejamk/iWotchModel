dir = getwd()
thispackage = paste(dir, "MarkovModel_0.1.0.zip",sep = "/")
install.packages(thispackage)
library(MarkovModel)
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
trans_control <-  rbind(c(1, NA, 2, NA, 3),
                                c(NA, NA, NA, NA, NA),
                                c(4, NA, 5, 6, 7),
                                c(8, NA, NA, 9, 10),
                                c(NA, NA, NA, NA, 11))

colnames(trans_intervention) <- rownames(trans_intervention) <- c("LTOT","IT","PT","SOLMP","DH")
colnames(trans_control) <- rownames(trans_control) <- c("LTOT","IT","PT","SOLMP","DH")

## parameterise the transition probbailities in the transition matrix
trans_matrix_intervention <- transition_matrix(5, trans_intervention,
                                               c("tp_LTOT_LTOT","tp_LTOT_IT", "tp_LTOT_PT",
                                                  "tp_LTOT_DH","tp_IT_LTOT","tp_IT_IT", "tp_IT_SOLMP",
                                                  "tp_IT_DH","tp_PT_LTOT", "tp_PT_PT", "tp_PT_SOLMP",
                                                  "tp_PT_DH","tp_SOLMP_LTOT", "tp_SOLMP_SOLMP",
                                                  "tp_SOLMP_DH","tp_DH_DH"), colnames(trans_intervention))

trans_matrix_control <- transition_matrix(5, trans_control,
                                               c("tp_LTOT_LTOT", "tp_LTOT_PT",
                                                 "tp_LTOT_DH","tp_PT_LTOT", "tp_PT_PT", "tp_PT_SOLMP",
                                                 "tp_PT_DH","tp_SOLMP_LTOT", "tp_SOLMP_SOLMP",
                                                 "tp_SOLMP_DH","tp_DH_DH"), colnames(trans_control))
length(trans_matrix_intervention$trans_matrix)
 param_file = system.file("extdata","table_param.csv", package = "iWotchModel")
#
 tp_LTOT_LTOT <- get_parameter_read("tp_LTOT_LTOT", param_file, "Intervention")
#param_interv <- define_parameters(

#)#

