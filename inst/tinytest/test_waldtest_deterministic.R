# A: test of equivalence between fwildclusterboot and wildboottestjlr
# testing via the tinytest package.

# don't run tests on CRAN
run_tests <- length(strsplit(packageDescription("fwildclusterboot")$Version, "\\.")[[1]]) > 2

if(run_tests){

  reltol <- 0.002

  N <- 10000
  #seed <- 871239345
  seed <- 875784

  data1 <<- wildboottestjlr:::create_data(N = 10000,
                                          N_G1 = 20,
                                          icc1 = 0.5,
                                          N_G2 = 20,
                                          icc2 = 0.2,
                                          numb_fe1 = 10,
                                          numb_fe2 = 10,
                                          seed = 90864369,
                                          weights = 1:N / N)

  lm_fit <- lm(proposition_vote ~ treatment  + log_income ,
               data = data1)



  lm_fit_weights <- lm(proposition_vote ~ treatment  + log_income  ,
                       weights = data1$weights,
                       data = data1)
  lm_fits <- list(ols = lm_fit, wls = lm_fit_weights)


  cat("Part 1: Large B Tests", "\n")

  object <- lm_fit
  type <- "rademacher"
  p_val_type <- "two-tailed"
  impose_null <- TRUE

  for(object in lm_fits){

    cat("start ols/wls", "\n")
    wildboottestjlr::set_julia_seed(12345)
    set.seed(12391786)
    dqrng::dqset.seed(8723467)

    for(type in c("rademacher", "webb", "mammen", "norm")){

      for(p_val_type in c("two-tailed", "equal-tailed", ">", "<")){

        for(impose_null in c(TRUE, FALSE)){

          cat(paste("type:", type, "p-val:", p_val_type, "null imposed:", impose_null), "\n")


            # one hypothesis
            R <- clubSandwich::constrain_zero(constraints = 2, coefs = coef(object))
            boot_jl <- wildboottestjlr::waldtest(object, R = R, clustid = "group_id1", B = 99999, type = type, p_val_type = p_val_type, impose_null = impose_null)
            clubSw <- clubSandwich::coef_test(obj = object, vcov = clubSandwich::vcovCR(obj = object,
                                                                                              cluster = data1$group_id1,
                                                                                               type = "CR1S"))
            expect_equivalent(round(boot_jl$t_stat, 5), round(clubSw$tstat[2],5))

            # two hypotheses
            # R <- clubSandwich::constrain_zero(constraints = 1:2, coefs = coef(object))
            # boot_jl <- wildboottestjlr::waldtest(object,
            #                                      R = R,
            #                                      clustid = "group_id1",
            #                                      B = 99999,
            #                                      type = type,
            #                                      p_val_type = p_val_type,
            #                                      impose_null = impose_null)
            # clubSw <- clubSandwich::Wald_test(obj = object,
            #                                   constraints = R,
            #                                   vcov = clubSandwich::vcovCR(obj = object,
            #                                                               cluster = data1$group_id1,
            #                                                               type = "CR1S"))
            # expect_equivalent(round(boot_jl$t_stat, 5), round(clubSw$Fstat,5))

            # test <- WildBootTests$wildboottest(R, r = c(0, 0), resp=data1$proposition_vote, predexog=cbind(1, data1$treatment, data1$log_income), clustid=data1$group_id1)
            # WildBootTests$teststat(test) # [1] 209.5094
            # WildBootTests$CI(test) # <0 x 0 matrix>

        }

      }


    }

  }



}
