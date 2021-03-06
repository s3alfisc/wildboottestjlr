#' Fast wild cluster bootstrap inference for object of class lm
#'
#' `waldtest.lm` is a S3 method that allows for fast wild cluster
#' bootstrap inference of multivariate hypotheses for objects of class lm by
#' implementing the fast wild bootstrap algorithm developed in Roodman et al., 2019.
#'
#' @param object An object of class lm
#' @param clustid A character vector containing the names of the cluster variables
#' @param B Integer. The number of bootstrap iterations. When the number of clusters is low,
#'        increasing B adds little additional runtime.
#' @param bootcluster A character vector. Specifies the bootstrap clustering variable or variables. If more
#'        than one variable is specified, then bootstrapping is clustered by the intersections of
#'        clustering implied by the listed variables. To mimic the behavior of stata's boottest command,
#'        the default is to cluster by the intersection of all the variables specified via the `clustid` argument,
#'        even though that is not necessarily recommended (see the paper by Roodman et al cited below, section 4.2).
#'        Other options include "min", where bootstrapping is clustered by the cluster variable with the fewest clusters.
#' @param sign_level A numeric between 0 and 1 which sets the significance level
#'        of the inference procedure. E.g. sign_level = 0.05
#'        returns 0.95% confidence intervals. By default, sign_level = 0.05.
#' @param conf_int A logical vector. If TRUE, boottest computes confidence
#'        intervals by p-value inversion. If FALSE, only the p-value is returned.
#' @param rng An integer. Controls the random number generation, which is handled via the `StableRNG()` function from the `StableRNGs` Julia package.
#' @param R Hypothesis Vector or Matrix giving linear combinations of coefficients. Must be either a vector of length k or a matrix of dimension q x k, where q is the number
#'        of joint hypotheses and k the number of estimated coefficients.
#' @param beta0 A vector of length q, where q is the number of tested hypotheses. Shifts the null hypothesis
#'        H0: param = beta0 vs H1: param != beta0. If not provided, a vector of zeros of length q.
#' @param type character or function. The character string specifies the type
#'        of boostrap to use: One of "rademacher", "mammen", "norm", "gamma"
#'        and "webb". Alternatively, type can be a function(n) for drawing
#'        wild bootstrap factors. "rademacher" by default.
#'        For the Rademacher and Mammen distribution, if the number of replications B exceeds
#'        the number of possible draw ombinations, 2^(#number of clusters), then `boottest()`
#'        will use each possible combination once (enumeration).
#' @param impose_null Logical. Controls if the null hypothesis is imposed on
#'        the bootstrap dgp or not. Null imposed `(WCR)` by default.
#'        If FALSE, the null is not imposed `(WCU)`
#' @param p_val_type Character vector of length 1. Type of p-value.
#'        By default "two-tailed". Other options include "equal-tailed", ">" and "<".
#' @param tol Numeric vector of length 1. The desired accuracy
#'        (convergence tolerance) used in the root finding procedure to find the confidence interval.
#'        Relative tolerance of 1e-6 by default.
#' @param na_omit Logical. If TRUE, `boottest()` omits rows with missing
#'        variables in the cluster variable that have not previously been deleted
#'        when fitting the regression object (e.g. if the cluster variable was not used
#'        when fitting the regression model).
#' @param floattype Float32 by default. Other optio: Float64. Should floating point numbers in Julia be represented as 32 or 64 bit?
#' @param fweights Logical. FALSE by default, TRUE for frequency weights.
#' @param getauxweights Logical. FALSE by default. Whether to save auxilliary weight matrix (v)
#' @param t_boot Logical. Should bootstrapped t-statistics be returned?
#' @param turbo Logical scalar, FALSE by default. Whether to exploit acceleration of the LoopVectorization package: slower on first use in a session, faster after
#' @param maxmatsize NULL by default = no limit. Else numeric scalar to set the maximum size of auxilliary weight matrix (v), in gigabytes
#' @param bootstrapc Logical scalar, FALSE by default. TRUE  to request bootstrap-c instead of bootstrap-t
#' @param ssc An object of class `boot_ssc.type` obtained with the function \code{\link[fwildclusterboot]{boot_ssc}}. Represents how the small sample adjustments are computed. The defaults are `adj = TRUE, fixef.K = "none", cluster.adj = "TRUE", cluster.df = "conventional"`.
#'             You can find more details in the help file for `boot_ssc()`. The function is purposefully designed to mimic fixest's \code{\link[fixest]{ssc}} function.
#' @param ... Further arguments passed to or from other methods.
#'
#' @import JuliaConnectoR
#' @importFrom dreamerr check_arg validate_dots
#' @importFrom stats as.formula coef model.matrix model.response model.weights residuals rlnorm rnorm update
#'
#' @method waldtest lm
#'
#' @return An object of class \code{waldtest}
#'
#' \item{p_val}{The bootstrap p-value.}
#' \item{conf_int}{The bootstrap confidence interval.}
#' \item{param}{The tested parameter.}
#' \item{N}{Sample size. Might differ from the regression sample size if the
#'          cluster variables contain NA values.}
#' \item{B}{Number of Bootstrap Iterations.}
#' \item{clustid}{Names of the cluster Variables.}
#' \item{N_G}{Dimension of the cluster variables as used in boottest.}
#' \item{sign_level}{Significance level used in boottest.}
#' \item{type}{Distribution of the bootstrap weights.}
#' \item{t_stat}{The original test statistics - either imposing the null or not - with small sample correction `G / (G-1)`.}
#' \item{test_vals}{All t-statistics calculated while calculating the
#'       confidence interval.}
#'  \item{t_boot}{All bootstrap t-statistics.}
#' \item{regression}{The regression object used in boottest.}
#' \item{call}{Function call of boottest.}
#' \item{getauxweights}{The bootstrap auxiliary weights matrix v. Only returned if getauxweights = TRUE.}
#' \item{t_boot}{The bootstrapped t-statistics. Only returned if t_boot = TRUE.}
#'
#' @export
#'
#' @references Roodman et al., 2019, "Fast and wild: Bootstrap inference in
#'             STATA using boottest", The STATA Journal.
#'             (\url{https://journals.sagepub.com/doi/full/10.1177/1536867X19830877})
#' @references Cameron, A. Colin, Jonah B. Gelbach, and Douglas L. Miller. "Bootstrap-based improvements for inference with clustered errors." The Review of Economics and Statistics 90.3 (2008): 414-427.
#' @references MacKinnon, James G., and Matthew D. Webb. "The wild bootstrap for few (treated) clusters." The Econometrics Journal 21.2 (2018): 114-135.
#' @references MacKinnon, James. "Wild cluster bootstrap confidence intervals." L'Actualite economique 91.1-2 (2015): 11-33.
#' @references Webb, Matthew D. Reworking wild bootstrap based inference for clustered errors. No. 1315. Queen's Economics Department Working Paper, 2013.
#' @examples
#' \dontrun{
#'  library(wildboottestjlr)
#'  data(voters)
#'  lm_fit <-lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
#'           data = voters)
#' }

waldtest.lm <- function(object,
                        clustid,
                        B,
                        R,
                        beta0 = rep(0,nrow(R)),
                        bootcluster = "max",
                        conf_int = NULL,
                        rng = NULL,
                        sign_level = NULL,
                        type = "rademacher",
                        impose_null = TRUE,
                        p_val_type = "two-tailed",
                        tol = 1e-6,
                        na_omit = TRUE,
                        floattype = "Float32",
                        #small_sample_adjustment = TRUE,
                        fweights = FALSE,
                        getauxweights = FALSE,
                        t_boot = FALSE,
                        turbo = FALSE,
                        maxmatsize = NULL,
                        bootstrapc = FALSE,
                        ssc = boot_ssc(adj = TRUE,
                                       fixef.K = "none",
                                       cluster.adj = TRUE,
                                       cluster.df = "conventional"),
                        ...) {

  call <- match.call()
  dreamerr::validate_dots(stop = TRUE)

  check_arg(clustid, "character scalar | character vector")
  check_arg(B, "scalar integer")
  check_arg(sign_level, "scalar numeric")
  check_arg(conf_int, "logical scalar | NULL")
  check_arg(rng, "scalar integer | NULL")
  check_arg(R, "numeric vector | numeric matrix")
  check_arg(beta0, "numeric vector  | NULL")
  check_arg(bootcluster, "character vector")
  check_arg(tol, "numeric scalar")
  check_arg(floattype, "character scalar")
  # check_arg(small_sample_adjustment, "scalar logical")
  check_arg(fweights, "scalar logical")
  check_arg(getauxweights, "scalar logical")
  check_arg(t_boot, "scalar logical")
  check_arg(turbo, "scalar logical")
  check_arg(maxmatsize, "scalar integer | NULL")
  check_arg(bootstrapc, "scalar logical")
  check_arg(floattype, "charin(Float32, Float64")

  if(length(beta0) != nrow(R)){
    stop(paste("beta0 must be a vector of length nrow(R) = ", nrow(R), "but it is of length", length(beta0), "."))
  }

  # param required in preprocess
  param <- NULL

  # translate ssc into small_sample_adjustment
  if(ssc[['adj']] == TRUE && ssc[['cluster.adj']] == TRUE){
    small_sample_adjustment <- TRUE
  } else {
    small_sample_adjustment <- FALSE
  }

  if(ssc[['fixef.K']] != "none" || ssc[['cluster.df']] != "conventional"){
    message(paste("Currently, boottest() only supports fixef.K = 'none' and cluster.df = 'conventional'."))
  }

  # nrow_R <- nrow(R)
  # if(nrow_R > 1 && (is.null(conf_int) || conf_int == TRUE)){
  #   message(paste("The hypothesis tests", nrow_R, "joint hypothesis and conf_int = TRUE. boottest() does not compute confidence intervals for hypotheses with q>1, and therefore we set conf_int = FALSE."))
  #   # even better: manipulate call
  #   conf_int <- FALSE
  # }


  if(!(floattype %in% c("Float32", "Float64"))){
    stop("floattype needs either to be 'Float32' or 'Float64'.")
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



  if (((1 - sign_level) * (B + 1)) %% 1 != 0) {
    message(paste("Note: The bootstrap usually performs best when the
                  confidence level (here,", 1 - sign_level, "%)
                  times the number of replications plus 1
                  (", B, "+ 1 = ", B + 1, ") is an integer."))
  }


  # throw error if specific function arguments are used in lm() call
  call_object <- names(object$call)[names(object$call) != ""]
  banned_fun_args <- c("contrasts", "subset", "offset", "x", "y")
  if (sum(call_object %in% banned_fun_args) > 0) {
    stop(paste(
      "boottest.lm currently does not accept objects of type lm with
      function arguments",
      paste0(banned_fun_args[1:(length(banned_fun_args) - 1)], collapse = ", "),
      "and", banned_fun_args[length(banned_fun_args)], "."
    ),
    call. = FALSE
    )
  }

  # preprocess data: X, Y, weights, fixed effects
  #pracma::tic()
  preprocess <- preprocess(object = object,
                           cluster = clustid,
                           fe = NULL,
                           bootcluster = bootcluster,
                           na_omit = na_omit,
                           R = R)
  #pracma::toc()


  clustid_dims <- preprocess$clustid_dims

  clustid_fml <- as.formula(paste("~", paste(clustid, collapse = "+")))

  # number of clusters used in bootstrap - always derived from bootcluster
  N_G <- length(unique(preprocess$bootcluster[, 1]))
  N_G_2 <- 2^N_G
  # NOTE: no need to reset B in enumeration case -> handled by WildBootTests.jl ->
  # throws an error
  if (type %in% c("rademacher") & N_G_2 < B) {
    warning(paste("There are only", N_G_2, "unique draws from the rademacher distribution for", length(unique(preprocess$bootcluster[, 1])), "clusters. Therefore, B = ", N_G_2, " with full enumeration. Consider using webb weights instead."),
            call. = FALSE,
            noBreaks. = TRUE
    )
    warning(paste("Further, note that under full enumeration and with B =", N_G_2, "bootstrap draws, only 2^(#clusters - 1) = ", 2^(N_G - 1), " distinct t-statistics and p-values can be computed. For a more thorough discussion, see Webb `Reworking wild bootstrap based inference for clustered errors` (2013)."),
            call. = FALSE,
            noBreaks. = TRUE
    )
  }


  # send R objects to Julia
  # assign all values needed in WildBootTests.jl

  resp <- as.numeric(preprocess$Y)
  predexog <- preprocess$X
  if(is.matrix(preprocess$R)){
    R <- preprocess$R
  } else {
    R <- matrix(preprocess$R, 1, length(preprocess$R))
  }
  r <- beta0
  reps <- as.integer(B) # WildBootTests.jl demands integer


  # Order the columns of `clustid` this way:
  # 1. Variables only used to define bootstrapping clusters, as in the subcluster bootstrap.
  # 2. Variables used to define both bootstrapping and error clusters.
  # 3. Variables only used to define error clusters.
  # In the most common case, `clustid` is a single column of type 2.

  if(length(bootcluster == 1) && bootcluster == "max"){
    bootcluster_n <- clustid
  } else if(length(bootcluster == 1) && bootcluster == "min"){
    bootcluster_n <- names(preprocess$N_G[which.min(preprocess$N_G)])
  } else {
    bootcluster_n <- bootcluster
  }

  # only bootstrapping cluster: in bootcluster and not in clustid
  c1 <- bootcluster_n[which(!(bootcluster_n %in% clustid))]
  # both bootstrapping and error cluster: all variables in clustid that are also in bootcluster
  c2 <- clustid[which(clustid %in% bootcluster_n)]
  # only error cluster: variables in clustid not in c1, c2
  c3 <- clustid[which(!(clustid %in% c(c1, c2)))]
  all_c <- c(c1, c2, c3)
  #all_c <- lapply(all_c , function(x) ifelse(length(x) == 0, NULL, x))

  # note that c("group_id1", NULL) == "group_id1"
  clustid_mat <- (preprocess$model_frame[, all_c])
  clustid_df <- base::as.matrix(sapply(clustid_mat, as.integer))

  # `nbootclustvar::Integer=1`: number of bootstrap-clustering variables
  # `nerrclustvar::Integer=nbootclustvar`: number of error-clustering variables
  nbootclustvar <- ifelse(length(bootcluster) == 1 && bootcluster == "max", length(clustid), length(bootcluster))
  nerrclustvar <- length(clustid)

  #obswt <-  preprocess$weights      # if no weights provided: vector of ones
  feid <- preprocess$fixed_effect
  level <-  1 - sign_level
  getCI <- ifelse(is.null(conf_int) || conf_int == TRUE, TRUE, FALSE)
  imposenull <- ifelse(is.null(impose_null) || impose_null == TRUE, TRUE, FALSE)
  rtol <- tol
  small <- small_sample_adjustment

  ptype <- switch(p_val_type,
                  "two-tailed" = "symmetric",
                  "equal-tailed" = "equaltail",
                  "<" = "lower",
                  ">" = "upper",
                  ptype
  )

  auxwttype <- switch(type,
                      "rademacher" = "rademacher",
                      "mammen" = "mammen",
                      "norm" = "normal",
                      "webb" = "webb",
                      "gamma" = "gamma",
                      auxwttype
  )

  JuliaConnectoR::juliaEval('using WildBootTests')
  JuliaConnectoR::juliaEval('using Random')

  WildBootTests <- JuliaConnectoR::juliaImport("WildBootTests")
  #if(!is.null(rng)){
  rng_jl <- juliaEval(paste0("Random.MersenneTwister(", rng, ")"))
  #}

  eval_list <- list(floattype,
                    R,
                    r,
                    resp = resp,
                    predexog = predexog,
                    clustid = clustid_df,
                    nbootclustvar = nbootclustvar,
                    nerrclustvar = nerrclustvar,
                    level = level,
                    getCI = getCI,
                    imposenull = imposenull,
                    rtol = rtol,
                    small = small,
                    rng = rng_jl,
                    ptype = ptype,
                    auxwttype = auxwttype,
                    reps = reps,
                    getauxweights = getauxweights,
                    turbo = turbo,
                    bootstrapc = bootstrapc
  )

  if(mean(preprocess$weights == 1) != 1){
    eval_list[["obswt"]] <- preprocess$weights
    eval_list[["fweights"]] <- fweights
  }

  if(!is.null(maxmatsize)){
    eval_list[["maxmatsize"]] <- maxmatsize
  }

  #pracma::tic()
  wildboottest_res <- do.call(WildBootTests$wildboottest, eval_list)
  #pracma::toc()


  # collect results:
  p_val <- WildBootTests$p(wildboottest_res)
  if(getCI == TRUE){
    conf_int <- WildBootTests$CI(wildboottest_res)
  } else{
    conf_int <- NA
  }

  t_stat <- WildBootTests$teststat(wildboottest_res)
  if(t_boot == TRUE){
    t_boot <- WildBootTests$dist(wildboottest_res)
  }

  if(getauxweights == TRUE){
    getauxweights <- WildBootTests$auxweights(wildboottest_res)
  }

  plotpoints <- WildBootTests$plotpoints(wildboottest_res)
  plotpoints <- cbind(plotpoints$X[[1]], plotpoints$p)

  res_final <- list(
    #point_estimate = point_estimate,
    p_val = p_val,
    conf_int = conf_int,
    # p_test_vals = res_p_val$p_grid_vals,
    # test_vals = res_p_val$grid_vals,
    t_stat = t_stat,
    t_boot = t_boot,
    auxweights = getauxweights,
    #regression = res$object,
    N = preprocess$N,
    B = B,
    clustid = clustid,
    # depvar = depvar,
    N_G = preprocess$N_G,
    sign_level = sign_level,
    call = call,
    type = type,
    impose_null = impose_null,
    R = R,
    beta0 = beta0,
    plotpoints = plotpoints
  )


  class(res_final) <- "waldtest"

  invisible(res_final)
  #res_final
}
