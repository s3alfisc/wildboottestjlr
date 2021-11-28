# # test IV methods against boottest.stata via RStata
#
# library(RStata)
# library(ivreg)
# library(data.table)
#
# options("RStata.StataVersion" = 16)
# options("RStata.StataPath" = "\"C:\\Program Files\\Stata16\\StataIC-64\"")
#
# data("SchoolingReturns", package = "ivreg")
# data1 <- SchoolingReturns
#
# tol <- 2 * 0.01
# save_test_data_to <- "c:/Users/alexa/Dropbox/wildboottestjlr/"
# save_data <- paste0(save_test_data_to, "voters.csv")
#
# #set.seed(1)
# #data1 <<- fwildclusterboot:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
# fwrite(data1, save_data)
#
# ivreg_fit <- ivreg(log(wage) ~ education + age + ethnicity + smsa + south + parents14 |
#                      nearcollege + age  + ethnicity + smsa + south + parents14,
#                    data = data)
#
#
# test_1 <- "
# clear
# import delimited c:/Users/alexa/Dropbox/wildboottestjlr/voters.csv
# set seed 1
# quietly ivregress proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
# boottest treatment, reps(99999) cluster(group_id1 ) nograph nonull
# gen p_val = r(p)
# //gen conf_int = r(CI)
# "
#
# boot_ivreg <- boottest(object = ivreg_fit, B = 999, param = "education", clustid = "fameducation", type = "webb")
