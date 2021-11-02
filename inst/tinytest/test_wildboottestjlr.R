# A: test of equivalence between fwildclusterboot and wildboottestjlr
# testing via the tinytest package.
# 1) test boot_lm

# don't run tests automatically, else devtools::check() will fail
run <- FALSE

if(run){
  library(wildboottestjlr)
  library(tinytest)
  # if not yet run: julia_setup()
  # wildboottestjlr::wildboottestjlr_setup("C:/Users/alexa/AppData/Local/Programs/Julia-1.6.3/bin")

  reltol <- 0.02

  N <- 10000
  voters <- fwildclusterboot:::create_data(N = N,
                                           N_G1 = 40,
                                           icc1 = 0.5,
                                           N_G2 = 20,
                                           icc2 = 0.2,
                                           numb_fe1 = 10,
                                           numb_fe2 = 10,
                                           seed = 45,
                                           weights = 1:N
  )

  lm_fit <- lm(proposition_vote ~ treatment  + log_income, data = voters)

  # evaluate line by line within loop to see if tests are passed
  # type <- "rademacher"
  # note: bug in Julia with type = "norm"
  # second note: there seems to be a bug in the ptype argument - "upper" produces results equal to "symmetric" or "equal-tailed"

  for(type in c("rademacher", "webb", "mammen", "norm")){

    boot_r <- fwildclusterboot::boottest(lm_fit, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = "two-tailed")
    boot_jl1 <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = "two-tailed")

    expect_equal(boot_r$p_val, boot_jl1$p_val, tol = reltol)
    expect_equal(boot_r$conf_int, c(boot_jl1$conf_int), tol = reltol)

    boot_r <- fwildclusterboot::boottest(lm_fit, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = "equal-tailed")
    boot_jl2 <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = "equal-tailed")
    expect_equal(boot_r$p_val, boot_jl2$p_val, tol = reltol)
    expect_equal(boot_r$conf_int, c(boot_jl2$conf_int), tol = reltol)

    boot_r <- fwildclusterboot::boottest(lm_fit, clustid = "group_id1", B = 199999, param = "treatment", type = type, p_val_type = ">", conf_int = FALSE)
    boot_jl3 <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", B = 199999, param = "treatment", type = type, p_val_type = ">", conf_int = FALSE)
    expect_equal(boot_r$p_val, boot_jl3$p_val, tol = reltol)

    boot_r <- fwildclusterboot::boottest(lm_fit, clustid = "group_id1", B = 199999, param = "treatment", type = type, p_val_type = "<", conf_int = FALSE)
    boot_jl4 <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", B = 199999, param = "treatment", type = type, p_val_type = "<", conf_int = FALSE)
    expect_equal(boot_r$p_val, boot_jl4$p_val, tol = reltol)

    # multi-param hypotheses

    boot_r <- fwildclusterboot::boottest(lm_fit, clustid = "group_id1", B = 299999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = "two-tailed")
    boot_jl <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", B = 299999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = "two-tailed")
    expect_equal(boot_r$p_val, boot_jl$p_val, tol = reltol)
    expect_equal(boot_r$conf_int, c(boot_jl$conf_int), tol = reltol)

    boot_r <- fwildclusterboot::boottest(lm_fit, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = "equal-tailed")
    boot_jl <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = "equal-tailed")
    expect_equal(boot_r$p_val, boot_jl$p_val, tol = reltol)
    expect_equal(boot_r$conf_int, c(boot_jl$conf_int), tol = reltol)

    boot_r <- fwildclusterboot::boottest(lm_fit, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = ">", conf_int = FALSE)
    boot_jl <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = ">", conf_int = FALSE)
    expect_equal(boot_r$p_val, boot_jl$p_val, tol = reltol)

    boot_r <- fwildclusterboot::boottest(lm_fit, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = "<", conf_int = FALSE)
    boot_jl <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = "<", conf_int = FALSE)
    expect_equal(boot_r$p_val, boot_jl$p_val, tol = reltol)


  }

}
