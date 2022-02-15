# A: test of equivalence between fwildclusterboot and wildboottestjlr
# testing via the tinytest package.

# don't run testson CRAN
run_tests <- length(strsplit(packageDescription("fwildclusterboot")$Version, "\\.")[[1]]) > 2

if(run_tests){

  reltol <- 0.002

  N <- 1000
  #seed <- 871239345
  seed <- 87578

  data1 <<- wildboottestjlr:::create_data(N = 1000,
      N_G1 = 20,
      icc1 = 0.5,
      N_G2 = 20,
      icc2 = 0.2,
      numb_fe1 = 10,
      numb_fe2 = 10,
      seed = 90864369,
      weights = 1:N / N)


  feols_fit <- feols(proposition_vote ~ treatment  + log_income + year,
   data = data1)
  lm_fit <- lm(proposition_vote ~ treatment  + log_income + year,
      data = data1)

  feols_fit_weights <- feols(proposition_vote ~ treatment  + log_income + year,
                     data = data1,
                     weights = data1$weights)
  lm_fit_weights <- lm(proposition_vote ~ treatment  + log_income + year,
                      data = data1,
                      weights = data1$weights)



  N <- nrow(data1)
  k <- length(coef(lm_fit))
  G <- length(unique(data1$group_id1))



  type <- "rademacher"
  p_val_type <- "two-tailed"
  # impose_null <- TRUE
  wildboottestjlr::set_julia_seed(12345)
  set.seed(12391786)
  dqrng::dqset.seed(8723467)


# OLS


# 1) oneway clustering

# one hypothesis
R <- clubSandwich::constrain_zero(constraints = 2, coefs = coef(lm_fit))
boot_jl <- suppressWarnings(wildboottestjlr::waldtest(floattype = "Float64", lm_fit, R = R, clustid = "group_id1", B = 999, type = type, p_val_type = p_val_type, impose_null = impose_null))

#sW <- coeftest(object, vcov = sandwich::vcovCL(object, cluster = ~ group_id1))
wald_stat <- fixest::wald(feols_fit, "treatment", cluster = ~ group_id1)

expect_equivalent(boot_jl$t_stat, sqrt(wald_stat$stat))

# two hypotheses
R <- clubSandwich::constrain_zero(constraints = 1:2, coefs = coef(lm_fit))
boot_jl <- suppressWarnings(
  wildboottestjlr::waldtest(floattype = "Float64",
object = lm_fit,
R = R,
clustid = "group_id1",
B = 999,
type = type,
p_val_type = p_val_type,
impose_null = impose_null,
ssc = wildboottestjlr::boot_ssc(adj = TRUE,
        cluster.adj = TRUE))
)

wald_stat <- fixest::wald(feols_fit, "Inter|treatment", cluster = ~ group_id1)
expect_equivalent(boot_jl$t_stat, wald_stat$stat)


# one hypothesis
R <- clubSandwich::constrain_zero(constraints = 2, coefs = coef(lm_fit))
boot_jl <- suppressWarnings(wildboottestjlr::waldtest(floattype = "Float64",
      lm_fit,
      R = R,
      clustid = c("group_id1", "group_id2"),
      B = 999,
      type = type,
      p_val_type = p_val_type,
      impose_null = impose_null,
      ssc = boot_ssc(cluster.df = "min")))

#sW <- coeftest(object, vcov = sandwich::vcovCL(object, cluster = ~ group_id1))
wald_stat <- fixest::wald(feols_fit,
  "treatment",
  cluster = ~ group_id1 + group_id2,
  ssc = ssc(cluster.df = "conventional"))

expect_equivalent(boot_jl$t_stat, sqrt(wald_stat$stat))

# two hypotheses
R <- clubSandwich::constrain_zero(constraints = 1:2, coefs = coef(lm_fit))
boot_jl <- suppressWarnings(
  wildboottestjlr::waldtest(floattype = "Float64",
    object = lm_fit,
    R = R,
    clustid = "group_id1",
    B = 999,
    type = type,
    p_val_type = p_val_type,
    impose_null = impose_null,
    ssc = wildboottestjlr::boot_ssc(adj = TRUE,
cluster.adj = TRUE))
)

wald_stat <- fixest::wald(feols_fit, "Inter|treatment",
  cluster = ~ group_id1,
  cluster.df = "conventional")
expect_equivalent(boot_jl$t_stat, wald_stat$stat)



# WLS


# 1) oneway clustering

# one hypothesis
R <- clubSandwich::constrain_zero(constraints = 2, coefs = coef(lm_fit_weights))
boot_jl <- suppressWarnings(wildboottestjlr::waldtest(floattype = "Float64", lm_fit_weights, R = R, clustid = "group_id1", B = 999, type = type, p_val_type = p_val_type, impose_null = impose_null))

#sW <- coeftest(object, vcov = sandwich::vcovCL(object, cluster = ~ group_id1))
wald_stat <- fixest::wald(feols_fit_weights, "treatment", cluster = ~ group_id1)

expect_equivalent(boot_jl$t_stat, sqrt(wald_stat$stat))

# two hypotheses
R <- clubSandwich::constrain_zero(constraints = 1:2, coefs = coef(lm_fit_weights))
boot_jl <- suppressWarnings(
  wildboottestjlr::waldtest(floattype = "Float64",
                            object = lm_fit_weights,
                            R = R,
                            clustid = "group_id1",
                            B = 999,
                            type = type,
                            p_val_type = p_val_type,
                            impose_null = impose_null,
                            ssc = wildboottestjlr::boot_ssc(adj = TRUE,
                                                            cluster.adj = TRUE))
)

wald_stat <- fixest::wald(feols_fit_weights, "Inter|treatment", cluster = ~ group_id1)
expect_equivalent(boot_jl$t_stat, wald_stat$stat)


# one hypothesis
R <- clubSandwich::constrain_zero(constraints = 2, coefs = coef(lm_fit_weights))
boot_jl <- suppressWarnings(wildboottestjlr::waldtest(floattype = "Float64",
                                                      lm_fit_weights,
                                                      R = R,
                                                      clustid = c("group_id1", "group_id2"),
                                                      B = 999,
                                                      type = type,
                                                      p_val_type = p_val_type,
                                                      impose_null = impose_null,
                                                      ssc = boot_ssc(cluster.df = "min")))

#sW <- coeftest(object, vcov = sandwich::vcovCL(object, cluster = ~ group_id1))
wald_stat <- fixest::wald(feols_fit_weights,
                          "treatment",
                          cluster = ~ group_id1 + group_id2,
                          ssc = ssc(cluster.df = "conventional"))

expect_equivalent(boot_jl$t_stat, sqrt(wald_stat$stat))

# two hypotheses
R <- clubSandwich::constrain_zero(constraints = 1:2, coefs = coef(lm_fit_weights))
boot_jl <- suppressWarnings(
  wildboottestjlr::waldtest(floattype = "Float64",
                            object = lm_fit_weights,
                            R = R,
                            clustid = "group_id1",
                            B = 999,
                            type = type,
                            p_val_type = p_val_type,
                            impose_null = impose_null,
                            ssc = wildboottestjlr::boot_ssc(adj = TRUE,
                                                            cluster.adj = TRUE))
)

wald_stat <- fixest::wald(feols_fit_weights, "Inter|treatment",
                          cluster = ~ group_id1,
                          cluster.df = "conventional")
expect_equivalent(boot_jl$t_stat, wald_stat$stat)



}
