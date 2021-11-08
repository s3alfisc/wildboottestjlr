# library(ivreg)
# data("SchoolingReturns", package = "ivreg")
# data <- SchoolingReturns
# head(data)
# data$parents14 <- as.factor(data$parents14)
#
# object <- ivreg(log(wage) ~ education + poly(experience, 2) + ethnicity + smsa + south + parents14 |
#                nearcollege + poly(age, 2) + ethnicity + smsa + south + parents14,
#              data = data,
#              y = TRUE)
# class(data$south)
# clustid <- "south"


boottest.ivreg <- function(object,
                          clustid,
                          param,
                          B,
                          bootcluster = "max",
                          conf_int = NULL,
                          rng = NULL,
                          R = NULL,
                          beta0 = 0,
                          sign_level = NULL,
                          type = "rademacher",
                          impose_null = TRUE,
                          p_val_type = "two-tailed",
                          tol = 1e-6,
                          maxiter = 10,
                          na_omit = TRUE,
                          ...){


  # check inputs
  call <- match.call()
  dreamerr::validate_dots(stop = TRUE)

  check_arg(clustid, "character scalar | character vector")
  check_arg(param, "scalar character | character vector")
  check_arg(B, "scalar integer")
  check_arg(sign_level, "scalar numeric")
  check_arg(conf_int, "logical scalar | NULL")
  check_arg(rng, "scalar integer | NULL")
  check_arg(R, "NULL| scalar numeric | numeric vector")
  check_arg(beta0, "numeric scalar | NULL")
  check_arg(bootcluster, "character vector")
  check_arg(tol, "numeric scalar")
  check_arg(maxiter, "scalar integer")
  check_arg(tol, "scalar numeric")

  # if an rng value is provided, set the seed internally
  if(!is.null(rng)){
    rng_char <- paste0("rng = StableRNGs.StableRNG(", rng, ");")
    JuliaCall::julia_command(rng_char)
  }

  if(maxiter < 1){
    stop("The function argument maxiter needs to be larger than 1.",
         call. = FALSE)
  }

  if(tol < 0){
    stop("The function argument tol needs to be positive.",
         call. = FALSE)
  }

  if(!(p_val_type %in% c("two-tailed", "equal-tailed",">", "<"))){
    stop("The function argument p_val_type must be
         'two-tailed', 'equal-tailed','>' or '<'.",
         call. = FALSE)
  }

  if ((conf_int == TRUE || is.null(conf_int)) & B <= 100) {
    stop("The function argument B is smaller than 100. The number of bootstrap
          iterations needs to be 100 or higher in order to guarantee that the
          root finding procudure used to find the confidence set
          works properly.",
         call. = FALSE
    )
  }
  if (!is.null(sign_level) & (sign_level <= 0 || sign_level >= 1)) {
    stop("The function argument sign_level is outside of the unit interval
         (0, 1). Please specify sign_level so that it is within the
         unit interval.",
         call. = FALSE
    )
  }

  if (is.null(sign_level)) {
    sign_level <- 0.05
  }

  # which parametrs can be tested?
  if (mean(param %in% names(c(object$exogenous, object$endogenous)) != 1)) {
    stop(paste("The parameter", param, "is not included in the estimated model.
               Maybe you are trying to test for an interaction parameter?
               To see all model parameter names, run names(coef(model))."))
  }

  if(is.null(R)){
    R <- rep(1, length(param))
  } else {
    if(length(R) != length(param)){
      stop("The constraints vector must either be NULL or a numeric of the same length as the `param` input vector.")
    }
  }

  if (((1 - sign_level) * (B + 1)) %% 1 != 0) {
    message(paste("Note: The bootstrap usually performs best when the
                  confidence level (here,", 1 - sign_level, "%)
                  times the number of replications plus 1
                  (", B, "+ 1 = ", B + 1, ") is an integer."))
  }

  if(object$method != "OLS"){
    stop("Currently, only 2SLS is supported. Please set the function argument method to `OLS`.")
  }

  # throw error if specific function arguments are used in lm() call
  call_object <- names(object$call)[names(object$call) != ""]
  banned_fun_args <- c("contrasts", "subset", "offset", "instruments")
  if (sum(call_object %in% banned_fun_args) > 0) {
    stop(paste(
      "boottest.ivreg currently does not accept objects of type lm with
      function arguments",
      paste0(banned_fun_args[1:(length(banned_fun_args) - 1)], collapse = ", "),
      "and", banned_fun_args[length(banned_fun_args)], "."
    ),
    call. = FALSE
    )
  }

  # preprocess data: X, Y, weights, fixed effects
  preprocess <- preprocess2(object = object,
                            cluster = clustid,
                            fe = NULL,
                            param = param,
                            bootcluster = bootcluster,
                            na_omit = na_omit,
                            R = R)

  # assign all values needed in WildBootTest.jl
  JuliaCall::julia_assign("Y", preprocess$Y)
  JuliaCall::julia_assign("X_endog", preprocess$X_endog)
  JuliaCall::julia_assign("X_exog", preprocess$X_exog)
  JuliaCall::julia_assign("instruments", preprocess$instruments)

  R <- matrix(c(preprocess$R, rep(0, ncol(preprocess$instruments))), 1, length(preprocess$R) + ncol(preprocess$instruments))
  JuliaCall::julia_assign("R", R)
  JuliaCall::julia_assign("beta0", beta0)
  JuliaCall::julia_eval("H0 = (R, beta0)")  # create a julia tuple for null hypothesis
  JuliaCall::julia_assign("reps", as.integer(B)) # WildBootTest.jl demands integer

  # Order the columns of `clustid` this way:
  # 1. Variables only used to define bootstrapping clusters, as in the subcluster bootstrap.
  # 2. Variables used to define both bootstrapping and error clusters.
  # 3. Variables only used to define error clusters.
  # In the most common case, `clustid` is a single column of type 2.

  if(length(bootcluster == 1) && bootcluster == "max"){
    bootcluster <- clustid[which.max(preprocess$N_G)]
  } else if(length(bootcluster == 1) && bootcluster == "min"){
    bootcluster <- clustid[which.min(preprocess$N_G)]
  }

  c1 <- bootcluster[which(!(bootcluster %in% clustid))]
  c2 <- clustid[which(clustid %in% bootcluster)]
  c3 <- clustid[which(!(clustid %in% bootcluster))]
  all_c <- c(c1, c2, c3)
  #all_c <- lapply(all_c , function(x) ifelse(length(x) == 0, NULL, x))

  JuliaCall::julia_assign("nbootclustvar", length(bootcluster))
  JuliaCall::julia_assign("nerrclustvar", length(clustid))

  # note that c("group_id1", NULL) == "group_id1"
  clustid_mat <- (preprocess$model_frame[, all_c])
  clustid_mat <- as.matrix(sapply(clustid_mat, as.integer))

  JuliaCall::julia_assign("clustid", clustid_mat)

  JuliaCall::julia_assign("weights", preprocess$weights)
  JuliaCall::julia_assign("fixed_effect", preprocess$fixed_effect)
  JuliaCall::julia_assign("bootcluster", preprocess$bootcluster)
  JuliaCall::julia_assign("level", 1 - sign_level)
  JuliaCall::julia_assign("getCI", ifelse(is.null(conf_int) || conf_int == TRUE, TRUE, FALSE))
  JuliaCall::julia_assign("obswt", preprocess$weights) # check if this is a vector of ones or NULL if no weights specified
  JuliaCall::julia_assign("imposenull", ifelse(is.null(impose_null) || impose_null == TRUE, TRUE, FALSE))
  JuliaCall::julia_assign("maxiter", maxiter)
  JuliaCall::julia_assign("rtol", tol)

  # only for felm, feols
  # JuliaCall::julia_assign("feid", preprocess$fixed_effect)

  # if(float == "Float32"){
  #   JuliaCall::julia_command("T = Base.Float32;")
  # } else if(float == "Float64"){
  #   JuliaCall::julia_command("T = Base.Float64;")
  # }


  if(p_val_type == "two-tailed"){
    JuliaCall::julia_command("ptype = WildBootTest.symmetric;")
  } else if(p_val_type == "equal_tailed"){
    JuliaCall::julia_command("ptype = WildBootTest.equaltail;")
  } else if(p_val_type == ">"){
    JuliaCall::julia_command("ptype = WildBootTest.upper;")
  } else if(p_val_type == "<"){
    JuliaCall::julia_command("ptype = WildBootTest.lower;")
  }

  if(type == "rademacher"){
    JuliaCall::julia_command("v = WildBootTest.rademacher;")
  } else if(type == "mammen"){
    JuliaCall::julia_command("v = WildBootTest.mammen;")
  } else if(type == "norm"){
    JuliaCall::julia_command("v = WildBootTest.normal;")
  } else if(type == "webb"){
    JuliaCall::julia_command("v = WildBootTest.webb;")
  }

  JuliaCall::julia_eval("boot_res = wildboottest(
                                    (R, [beta0]);
                                    resp = Y,
                                    predexog = X_exog,
                                    predendog = X_endog,
                                    inst = instruments,
                                    clustid= clustid,
                                    nbootclustvar = nbootclustvar,
                                    nerrclustvar = nerrclustvar,
                                    reps = reps,
                                    auxwttype=v,
                                    ptype = ptype,
                                    getCI = getCI,
                                    level = level,
                                    imposenull = imposenull,
                                    rng = rng,
                                    rtol = rtol

                        )")


}
