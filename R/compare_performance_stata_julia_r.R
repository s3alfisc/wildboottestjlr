#' compare_performance <- function(){
#'
#' #' Function compares performance of Stata, Julia and R boottest implementations
#' #' Note that benchmarking Stata through RStata provides some overhead - the STATA
#' #' implementation is usually faster than reported here
#' #' Stand-alone STATA code can be found below the function
#'
#' library(fwildclusterboot)
#' library(data.table)
#' library(RStata)
#' library(wildboottestjlr)
#' library(bench) # for benchmarking of fwildclusterboot::boottest() and wildboottestjlr::boottest()
#' wildboottestjlr_setup("C:/Users/alexa/AppData/Local/Programs/Julia-1.6.3/bin")
#'
#' options("RStata.StataVersion" = 16)
#' #chooseStataBin()
#' options("RStata.StataPath" = "\"C:\\Program Files\\Stata16\\StataIC-64\"")
#'
#' # define benchmark functions
#' estimate_r_runtime <- function(B, nthreads, clustid){
#'
#'   res <- bench::mark(
#'   boot_lm1 <-  suppressWarnings(
#'     fwildclusterboot::boottest(
#'       object = lm_fit,
#'       clustid =  clustid,
#'       B = B,
#'       seed = 911,
#'       param = "treatment",
#'       conf_int = TRUE,
#'       sign_level = 0.05,
#'       nthreads = nthreads,
#'     )
#'   ),
#'   iterations = 1,
#'   time_unit = "s")
#'   res$median
#' }
#'
#' estimate_julia_runtime <- function(B, nthreads, clustid){
#'
#'   # for a fair comparison, pre-compile the julia code
#'   compile <- wildboottestjlr::boottest(
#'     object = lm_fit,
#'     clustid =  clustid,
#'     B = B,
#'     rng = 911,
#'     param = "treatment",
#'     conf_int = TRUE,
#'     sign_level = 0.05
#'   )
#'
#'   res <- bench::mark(
#'     boot_lm1 <-  suppressWarnings(
#'       wildboottestjlr::boottest(
#'         object = lm_fit,
#'         clustid =  clustid,
#'         B = B,
#'         rng = 911,
#'         param = "treatment",
#'         conf_int = TRUE,
#'         sign_level = 0.05
#'       )
#'     ),
#'     iterations = 1,
#'     time_unit = "s")
#'   res$median
#' }
#'
#'
#' estimate_stata_runtime <- function(B){
#'
#' stata_command <-
#' paste0("
#' cap program drop Tic
#' program define Tic
#' 	syntax, n(integer)
#' 	timer on `n'
#' end
#'
#' cap program drop Toc
#' program define Toc
#' 	syntax, n(integer)
#' 	timer off `n'
#' end
#'
#' clear
#' import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
#' set seed 1
#' quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
#'
#'
#' timer clear
#' local i = 0
#'
#' Tic, n(`++i')
#' boottest treatment, reps(",B, ") cluster(group_id1) nograph
#' Toc, n(`i')
#'
#'
#' drop _all
#' gen result = .
#' set obs `i'
#' timer list
#' forval j = 1/`i'{
#' 	replace result = r(t`j') if _n == `j'
#' }
#' //gen timing = result
#' ")
#'
#' timing_in_s <- RStata::stata(stata_command, data.out = TRUE)
#' timing_in_s
#'
#' }
#'
#'
#' # ----------------------------------------------------------------------------- #
#' # estimate
#'
#' # create the test data set and save it on disk
#' set.seed(1)
#'
#' # save test data on disk so that it can be passed to Stata
#' save_test_data_to <- "c:/Users/alexa/Dropbox/fwildclusterboot/"
#' save_data <- paste0(save_test_data_to, "voters.csv")
#'
#' data1 <<- fwildclusterboot:::create_data(N = 10000, N_G1 = 1000, icc1 = 0.01, N_G2 = 20, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
#' fwrite(data1, save_data)
#'
#' # same model as estimated in stata
#' lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
#'              data = data1)
#'
#' # oneway
#' B <- c(999, 9999, 99999)
#' res_stata <- lapply(B , function(x) estimate_stata_runtime(B = x))
#' res_r1 <- lapply(B , function(x) estimate_r_runtime(B = x, nthreads = 1, clustid = c("group_id1")))
#' res_r2 <- lapply(B , function(x) estimate_r_runtime(B = x, nthreads = 4,clustid = c("group_id1")))
#' res_julia <- lapply(B , function(x) estimate_julia_runtime(B = x, nthreads = 4,clustid = c("group_id1")))
#'
#' oneway_df <- data.frame(stata = unlist(res_stata),
#'            r1 = unlist(res_r1),
#'            r2 = unlist(res_r2),
#'            res_julia = unlist(res_julia))
#' oneway_df
#'
#' }
#'


# cap program drop Tic
# program define Tic
# syntax, n(integer)
# timer on `n'
# end
#
# cap program drop Toc
# program define Toc
# 	syntax, n(integer)
# 	timer off `n'
# end
#
# clear
# import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
# set seed 1
# quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
#
#
# timer clear
# local i = 0
#
# Tic, n(`++i')
# boottest treatment, reps(99999) cluster(group_id1 ) nograph
# Toc, n(`i')
#
# drop _all
# gen result = .
# set obs `i'
# timer list
# forval j = 1/`i'{
#   replace result = r(t`j') if _n == `j'
# }
