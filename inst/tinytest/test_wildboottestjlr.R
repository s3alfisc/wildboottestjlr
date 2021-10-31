# A: test of equivalence between fwildclusterboot and wildboottestjlr
# testing via the tinytest package.
# 1) test boot_lm


# if not yet run: julia_setup()
# library(wildboottestjlr)
# wildboottestjlr::wildboottestjlr_setup("C:/Users/alexa/AppData/Local/Programs/Julia-1.6.3/bin")

reltol <- 1e-2

N <- 1000
voters <- fwildclusterboot:::create_data(N = N,
                                        N_G1 = 80,
                                        icc1 = 0.5,
                                        N_G2 = 20,
                                        icc2 = 0.2,
                                        numb_fe1 = 10,
                                        numb_fe2 = 10,
                                        seed = 4,
                                        weights = 1:N
                                        )

lm_fit <- lm(proposition_vote ~ treatment  + log_income, data = voters)

# evaluate line by line within loop to see if tests are passed
# type <- "rademacher"
# note: bug in Julia with type = "norm"

for(type in c("rademacher", "webb", "mammen", "norm")){

  boot_r <- fwildclusterboot::boottest(lm_fit, clustid = "group_id1", B = 99999, param = "treatment", type = type)
  boot_jl <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", B = 99999, param = "treatment", type = type)
  expect_equal(boot_r$p_val, boot_jl$p_val, tol = reltol)
  expect_equal(boot_r$conf_int, c(boot_jl$conf_int), tol = reltol)

  boot_r <- fwildclusterboot::boottest(lm_fit, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type)
  boot_jl <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type)
  expect_equal(boot_r$p_val, boot_jl$p_val, tol = reltol)
  expect_equal(boot_r$conf_int, c(boot_jl$conf_int), tol = reltol)

}




