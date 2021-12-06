# test that methods for lm(), feols() and fixest produces equivalent results

run <- FALSE

if(run){
  library(wildboottestjlr)
  library(tinytest)
  library(lfe)
  library(fixest)

  # ------------------------------------------------------------------ #
  # test 1: default behavior
  # - Tests .1 - one fixed effect,
  # - Tests .2 - two fixed effects
  # - Part A1: no fixed effect
  #            - cluster vars in boottest
  #            - cluster vars in feols and fixest
  #            - in total: 5 boottest objects created
  # - Part B1: one fixed effect
  #            - cluster vars in boottest
  #            - cluster vars in feols and fixest
  #            - switch fe on and off
  #            - in total: 10 boottest objects created
  # - Part C1: two fixed effects
  #            - cluster vars in boottest
  #            - cluster vars in feols and fixest
  #            - switch fe on and off
  #            - in total: 10 boottest objects created
  # ------------------------------------------------------------------ #

  # library(wildboottestjlr)

  # ---------------------------------------------------------------------------------------------- #
  # Part 1: one cluster variable
  # ---------------------------------------------------------------------------------------------- #

  # ---------------------------------------------------------------------------------------------- #
  # Part A1: no fixed effect in model

  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
               data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
                     data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
                   data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  feols_fit_c <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
                       cluster = "group_id1",
                       data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  felm_fit_c <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration | 0 | 0 | group_id1,
                     data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

  # test with floattype64
  pracma::tic()
  boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64"))
  pracma::toc()

  boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64"))
  boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64"))
  boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), B = 999, rng = 911, param = "treatment", conf_int = TRUE, fedfadj = TRUE, floattype = "Float64"))
  boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE, fedfadj = TRUE, floattype = "Float64"))

  lapply(list("point_estimate", "p_val", "t_stat", "conf_int"),
         function(stat){
           lapply(list(boot_fixest, boot_felm, boot_fixest_c, boot_felm_c),
                  function(x)
                    expect_equivalent(boot_lm[[stat]], x[[stat]]))
         }
  )

  # ---------------------------------------------------------------------------------------------- #
  # Part B1: one fixed effect in model

  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + as.factor(Q1_immigration) ,
               data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration,
                     data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration,
                   data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  feols_fit_c <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration,
                       cluster = "group_id1",
                       data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  felm_fit_c <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration  | 0 | group_id1,
                     data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))


  boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64"))
  boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64"))
  boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64"))
  boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), B = 999, rng = 911, param = "treatment", conf_int = TRUE, fedfadj = FALSE, floattype = "Float64"))
  boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE, fedfadj = FALSE, floattype = "Float64"))

  boot_fixest_fe <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), fe = "Q1_immigration", B = 999, rng = 911, param = "treatment", conf_int = TRUE, fedfadj = FALSE, floattype = "Float64"))
  boot_felm_fe <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", fe = "Q1_immigration", B = 999, rng = 911, param = "treatment", conf_int = TRUE, fedfadj = FALSE, floattype = "Float64"))
  boot_fixest_c_fe <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), fe = "Q1_immigration", B = 999, rng = 911, param = "treatment", conf_int = TRUE, fedfadj = FALSE, floattype = "Float64"))
  boot_felm_c_fe <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", fe = "Q1_immigration", B = 999, rng = 911, param = "treatment", conf_int = TRUE, fedfadj = FALSE, floattype = "Float64"))
  boot_felm_c_fe1 <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", fe = "Q1_immigration", B = 999, rng = 911, param = "treatment", conf_int = TRUE, fedfadj = TRUE, floattype = "Float64"))

  # no fixed effects: exactly equal
  lapply(list("point_estimate", "p_val", "t_stat", "conf_int"),
         function(stat){
           lapply(list(boot_fixest, boot_felm, boot_fixest_c, boot_felm_c),
                  function(x)
                    expect_equivalent(boot_lm[[stat]], x[[stat]]))
         }
  )

  # fixed effects: exactly equal
  lapply(list("point_estimate", "p_val", "t_stat", "conf_int"),
         function(stat){
           lapply(list(boot_felm_fe, boot_fixest_c_fe, boot_felm_c_fe),
                  function(x)
                    expect_equivalent(boot_fixest_fe[[stat]], x[[stat]]))
         }
  )

  # fixed effects: almost equal to boot_lm
  lapply(list("point_estimate", "p_val", "t_stat", "conf_int"),
         function(stat){
           lapply(list(boot_fixest_fe, boot_felm_fe, boot_fixest_c_fe, boot_felm_c_fe),
                  function(x)
                    expect_equivalent(boot_lm[[stat]], x[[stat]], tol = 1e-02))
         }
  )


  # ---------------------------------------------------------------------------------------------- #
  # Part C1: two fixed effects in model

  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration + Q2_defense,
               data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense,
                     data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense,
                   data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  feols_fit_c <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense,
                       cluster = "group_id1",
                       data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  felm_fit_c <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense | 0 | group_id1,
                     data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

  boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64"))
  boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64"))
  boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64"))
  boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), B = 999, rng = 911, param = "treatment", conf_int = TRUE, fedfadj = FALSE, floattype = "Float64"))
  boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE, fedfadj = FALSE, floattype = "Float64"))

  boot_lm_fe <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64"))
  boot_fixest_fe <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), fe = "Q1_immigration", B = 999, rng = 911, param = "treatment", conf_int = TRUE, fedfadj = FALSE, floattype = "Float64"))
  boot_felm_fe <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", fe = "Q1_immigration", B = 999, rng = 911, param = "treatment", conf_int = TRUE, fedfadj = FALSE, floattype = "Float64"))
  boot_fixest_c_fe <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), fe = "Q1_immigration", B = 999, rng = 911, param = "treatment", conf_int = TRUE, fedfadj = FALSE, floattype = "Float64"))
  boot_felm_c_fe <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", fe = "Q1_immigration", B = 999, rng = 911, param = "treatment", conf_int = TRUE, fedfadj = FALSE, floattype = "Float64"))

  # for(stat in c("point_estimate", "p_val", "t_stat", "conf_int")){
  #   for(x in list(boot_fixest, boot_felm, boot_fixest_c, boot_felm_c)){
  #     expect_equivalent(boot_lm[[stat]], x[[stat]])
  #   }
  # }

  # no fixed effects: exactly equal
  lapply(list("point_estimate", "p_val", "t_stat", "conf_int"),
         function(stat){
           lapply(list(boot_fixest, boot_felm, boot_fixest_c, boot_felm_c),
                  function(x)
                    expect_equivalent(boot_lm[[stat]], x[[stat]]))
         }
  )

  # fixed effects: exactly equal
  lapply(list("point_estimate", "p_val", "t_stat", "conf_int"),
         function(stat){
           lapply(list(boot_felm_fe, boot_fixest_c_fe, boot_felm_c_fe),
                  function(x)
                    expect_equivalent(boot_fixest_fe[[stat]], x[[stat]]))
         }
  )

  # fixed effects: almost equal to boot_lm
  lapply(list("point_estimate", "p_val", "t_stat", "conf_int"),
         function(stat){
           lapply(list(boot_fixest_fe, boot_felm_fe, boot_fixest_c_fe, boot_felm_c_fe),
                  function(x)
                    expect_equivalent(boot_lm[[stat]], x[[stat]], tol = 1e-02))
         }
  )



  # ---------------------------------------------------------------------------------------------- #
  # Part 2: two cluster variables
  # ---------------------------------------------------------------------------------------------- #

  # ---------------------------------------------------------------------------------------------- #
  # Part A2: no fixed effect in model

  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
               data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
                     data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
                   data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  feols_fit_c <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
                       cluster = "group_id1",
                       data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  felm_fit_c <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration | 0 | 0 | group_id1,
                     data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

  boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64"))
  boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64"))
  boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64"))
  boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64"))
  boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64"))

  lapply(list("point_estimate", "p_val", "t_stat", "conf_int"),
         function(stat){
           lapply(list(boot_fixest, boot_felm, boot_fixest_c, boot_felm_c),
                  function(x)
                    expect_equivalent(boot_lm[[stat]], x[[stat]]))
         }
  )


  # ---------------------------------------------------------------------------------------------- #
  # Part B2: one fixed effect in model

  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
               data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration,
                     data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration,
                   data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  feols_fit_c <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration,
                       cluster = "group_id1",
                       data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  felm_fit_c <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration  | 0 | group_id1,
                     data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

  boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64"))
  boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64", fedfadj = FALSE))
  boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64", fedfadj = FALSE))
  boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64", fedfadj = FALSE))
  boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64", fedfadj = FALSE))

  boot_lm_fe <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64"))
  boot_fixest_fe <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), fe = "Q1_immigration", B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64", fedfadj = FALSE))
  boot_felm_fe <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", fe = "Q1_immigration", B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64", fedfadj = FALSE))
  boot_fixest_c_fe <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), fe = "Q1_immigration", B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64", fedfadj = FALSE))
  boot_felm_c_fe <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", fe = "Q1_immigration", B = 999, rng = 911, param = "treatment", conf_int = TRUE, floattype = "Float64", fedfadj = FALSE))

  # no fixed effects: exactly equal
  lapply(list("point_estimate", "p_val", "t_stat", "conf_int"),
         function(stat){
           lapply(list(boot_fixest, boot_felm, boot_fixest_c, boot_felm_c),
                  function(x)
                    expect_equivalent(boot_lm[[stat]], x[[stat]]))
         }
  )

  # fixed effects: exactly equal
  lapply(list("point_estimate", "p_val", "t_stat", "conf_int"),
         function(stat){
           lapply(list(boot_felm_fe, boot_fixest_c_fe, boot_felm_c_fe),
                  function(x)
                    expect_equivalent(boot_fixest_fe[[stat]], x[[stat]]))
         }
  )

  # fixed effects: almost equal to boot_lm
  lapply(list("point_estimate", "p_val", "t_stat", "conf_int"),
         function(stat){
           lapply(list(boot_fixest_fe, boot_felm_fe, boot_fixest_c_fe, boot_felm_c_fe),
                  function(x)
                    expect_equivalent(boot_lm[[stat]], x[[stat]], tol = 1e-02))
         }
  )




  # ---------------------------------------------------------------------------------------------- #
  # Part C2: two fixed effects in model

  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration + Q2_defense,
               data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense,
                     data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense,
                   data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  feols_fit_c <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense,
                       cluster = "group_id1",
                       data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
  felm_fit_c <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense | 0 | group_id1,
                     data = wildboottestjlr:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

  boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE))
  boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, rng = 911, param = "treatment", conf_int = TRUE))
  boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE))
  boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), B = 999, rng = 911, param = "treatment", conf_int = TRUE))
  boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE))

  boot_lm_fe <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 999, rng = 911, param = "treatment", conf_int = TRUE))
  boot_fixest_fe <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), fe = "Q1_immigration", B = 999, rng = 911, param = "treatment", conf_int = TRUE))
  boot_felm_fe <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", fe = "Q1_immigration", B = 999, rng = 911, param = "treatment", conf_int = TRUE))
  boot_fixest_c_fe <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), fe = "Q1_immigration", B = 999, rng = 911, param = "treatment", conf_int = TRUE))
  boot_felm_c_fe <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", fe = "Q1_immigration", B = 999, rng = 911, param = "treatment", conf_int = TRUE))


  # no fixed effects: exactly equal
  lapply(list("point_estimate", "p_val", "t_stat", "conf_int"),
         function(stat){
           lapply(list(boot_fixest, boot_felm, boot_fixest_c, boot_felm_c),
                  function(x)
                    expect_equivalent(boot_lm[[stat]], x[[stat]], tol = 1e-02))
         }
  )

  # fixed effects: exactly equal
  lapply(list("point_estimate", "p_val", "t_stat", "conf_int"),
         function(stat){
           lapply(list(boot_felm_fe, boot_fixest_c_fe, boot_felm_c_fe),
                  function(x)
                    expect_equivalent(boot_fixest_fe[[stat]], x[[stat]]))
         }
  )

  # fixed effects: almost equal to boot_lm
  lapply(list("point_estimate", "p_val", "t_stat", "conf_int"),
         function(stat){
           lapply(list(boot_fixest_fe, boot_felm_fe, boot_fixest_c_fe, boot_felm_c_fe),
                  function(x)
                    expect_equivalent(boot_lm[[stat]], x[[stat]], tol = 1e-02))
         }
  )


}
