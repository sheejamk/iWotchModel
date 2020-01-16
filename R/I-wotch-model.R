library(MarkovModel)
## initiliase the health states
LTOT <- health_state("LTOT","cost_LTOT","util_LTOT")
IT <- health_state("IT","cost_IT","util_IT")
PT <- health_state("PT","cost_PT","util_PT")
SOLMP <- health_state("SOLMP","cost_SOLMP","util_SOLMP")
DH <- health_state("DH",0,0)

## allowed transitions between health states the health states
trans_intervention <- rbind(c(1, 2, 3, 4, 5),
                       c(6, 7, NA, 8, 9),
                       c(10, NA, 11, 12, 13),
                       c(14, NA, 15, 16, 17),
                       c(NA, NA, NA, NA, 18))
trans_control <-  rbind(c(1, NA, 2, 3, 4),
                                c(NA, NA, NA, NA, NA),
                                c(5, NA, 6, 7, 8),
                                c(9, NA, 10, 11, 12),
                                c(NA, NA, NA, NA, 13))

colnames(trans_intervention) <- rownames(trans_intervention) <- c("LTOT","IT","PT","SOLMP","DH")
colnames(trans_control) <- rownames(trans_control) <- c("LTOT","IT","PT","SOLMP","DH")

## parameterise the transition probbailities in the transition matrix
trans_matrix_intervention <- transition_matrix(5, trans_intervention,
                                               c("tp_LTOT_LTOT","tp_LTOT_IT", "tp_LTOT_PT", "tp_LTOT_SOLMP",
                                                  "tp_LTOT_DH","tp_IT_LTOT","tp_IT_IT", "tp_IT_SOLMP",
                                                  "tp_IT_DH","tp_PT_LTOT", "tp_PT_PT", "tp_PT_SOLMP",
                                                  "tp_PT_DH","tp_SOLMP_LTOT", "tp_SOLMP_PT", "tp_SOLMP_SOLMP",
                                                  "tp_SOLMP_DH","tp_DH_DH"), colnames(trans_intervention))

trans_matrix_control <- transition_matrix(5, trans_control,
                                               c("tp_LTOT_LTOT", "tp_LTOT_PT", "tp_LTOT_SOLMP",
                                                 "tp_LTOT_DH","tp_PT_LTOT", "tp_PT_PT", "tp_PT_SOLMP",
                                                 "tp_PT_DH","tp_SOLMP_LTOT", "tp_SOLMP_PT", "tp_SOLMP_SOLMP",
                                                 "tp_SOLMP_DH","tp_DH_DH"), colnames(trans_control))

