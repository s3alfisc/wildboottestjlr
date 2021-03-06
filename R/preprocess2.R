#' function that pre-processes regression objects of type lm, fixest and feols
#' @param object An object of class lm, fixest or felm
#' @param cluster A vector with the names of the clusters
#' @param fe A character scalar - fixed effect to be projected out, or NULL
#' @param param The univariate coefficients for which a hypothesis is to be tested
#' @param bootcluster The bootstrap sampling cluster.
#' @param na_omit Logical. If TRUE, `boottest()` omits rows with missing variables that are added to the model via the `cluster` argument in `boottest()`
#' @param R Hypothesis Vector giving linear combinations of coefficients. Must be either NULL or a vector of the same length as `param`. If NULL, a vector of ones of length param.
#' @return List containing preprocessed data for boottest estimation
#' @importFrom dreamerr check_arg
#' @importFrom Formula as.Formula
#' @noRd

preprocess <- function(object, cluster, fe, param, bootcluster, na_omit, R) {

  # ---------------------------------------------------------------------------- #
  # Step 1: preprocessing of call

  check_arg(cluster, "character scalar | character vector")
  check_arg(fe, "character scalar | NULL")
  check_arg(param, "character vector | character vector | NULL")
  check_arg(bootcluster, "character vector | NULL")
  check_arg(R, "numeric vector | numeric scalar| numeric matrix")

  if (class(object) == "fixest") {
    of <- object$call

    o <- match(c("fml", "data", "weights", "cluster", "fixef"), names(of), 0L)
    # keep only required arguments
    of <- of[c(1L, o)]
    # add argument to function
    of$drop.unused.levels <- TRUE

    # create formula objects by adding fixed effects and clusters from call
    # add potential other cluster variables from cluster argument
    formula <- eval(of$fml)

    # formula manipulation
    # Step 1: get formula without fixed effects and clusters
    # Step 2: add fixed effects from fixest
    # Step 3: add cluster variables from fixest
    # Step 4: add additional cluster variables specified in boottest()
    # Step 5: add additional bootcluster variables specified in boottest()

    # combine fixed effects in formula with main formula
    # note: you get a warning here if rhs = 2 is empty (no fixed effect specified via fml)
    formula <- suppressWarnings(formula(Formula::as.Formula(formula), lhs = 1, rhs = c(1, 2), collapse = TRUE))

    # if there are fixed effects specified not via formula, but via fixef arg
    if (!is.null(eval(of$fixef))) {
      # add additional fixed effects specified in fixef argument of feols()
      formula <- update(formula, paste("~ . +", paste(eval(of$fixef), collapse = " + ")))
    }

    # formula with only depvar and covariates, needed to construct design matrix X
    formula_X <- formula

    # drop fe from formula if required
    if(!is.null(fe)){
      formula_X <- update(formula_X, paste("~ . -", paste(fe, collapse = "+")))
    }

    # add cluster variables specified in feols-cluster and boottest-cluster arguments
    if (!is.null(eval(of$cluster))) {
      if(class(eval(of$cluster)) == "formula"){
        add_cluster <- unlist(strsplit(deparse(of$cluster), "[~]"))[2]
        formula <- update(formula, paste("~ . +", paste(add_cluster, collapse = "+")))
      } else if(class(eval(of$cluster)) == "character"){
        formula <- update(formula, paste("~ . +", paste(eval(of$cluster), collapse = "+")))
      } else {
        deparse_data <- unlist(strsplit(deparse(of$data), "[$]"))
        deparse_cluster <- unlist(strsplit(deparse(of$cluster), "[$]"))
        add_cluster <- deparse_cluster[-(which(deparse_cluster %in% deparse_data))]
        formula <- update(formula, paste("~ . +", paste(add_cluster, collapse = "+")))
      }
    }

    if (!is.null(cluster)) {
      formula <- update(formula, paste("~ . +", paste(cluster, collapse = "+")))
    }

    # add bootcluster variables
    if(sum(bootcluster %in% c(NULL, "max", "min")) == 0){
      formula <- update(formula, paste("~ . +", paste(bootcluster, collapse = "+")))
    }

    of$formula <- as.call(formula)

    o <- match(c("formula", "data", "weights"), names(of), 0L)
    of <- of[c(1L, o)]
    of[[1L]] <- quote(stats::model.frame)
    # of is a data.frame that contains all variables: depvar, X, fixed effects and clusters specified
    # in feols and via fe argument

    of <- eval(of, parent.frame())

    # check if one of the fixed effects or cluster variables is not of
    # type character or factor

    fixedid <- object$fixef_vars[which(object$fixef_vars!= fe)]
    # are fixed effects neither character nor factor?
    j <- which(!(sapply(data.frame(of[,c(fixedid)]), class) %in%  c("factor")))

    # if only one fixed effect & if it is not a factor, `of[,c(fixedid)]` is not a data.frame but a vector
    if(length(j) == 1){
      of[, c(fixedid)] <- factor(of[, c(fixedid)])
    } else if(length(j) > 1) {
      of[, c(fixedid)][,j] <- lapply(j, function(x) factor(of[, c(fixedid)][,x]))
    }

    # are all integer/ numeric variables now characters?
    #j <- !(sapply(data.frame(of[,c(fixedid)]), class) %in%  c("factor"))

    # if at least one j evaluates to "TRUE"
    # not covered by test coverage - case should never occur
    #if(sum(j) > 0){
    #  stop(paste("The fixed effects variable(s)", paste(fixedid[j], collapse = " & "), "is/are not factor variables. This should have been fixed internally but apparently wasn't. Please report the bug!"))
    #}
    N_model <- object$nobs
    model_param_names <- c(names(coef(object)), object$fixef)

  } else if (class(object) == "felm") {

    of <- object$call
    o <- match(c("formula", "data", "weights"), names(of), 0L)
    # keep only required arguments
    of <- of[c(1L, o)]
    of$drop.unused.levels <- TRUE

    # create formula objects by adding fixed effects and clusters from call
    # add potential other cluster variables from cluster argument
    if (suppressWarnings(formula(Formula::as.Formula(eval(of$formula)), lhs = 0, rhs = 3)) != "~0") {
      stop("IV estimation is currently not supported by boottest()")
    }

    # formula: model formula plus additional additional cluster variables specified via boottest()

    formula <- suppressWarnings(formula(Formula::as.Formula(eval(of$formula)), lhs = 1, rhs = c(1, 2, 4), collapse = TRUE))

    # formula with only depvar and covariates, needed to construct design matrix X
    formula_X <- suppressWarnings(formula(Formula::as.Formula(eval(of$formula)), lhs = 1, rhs = c(1, 2), collapse = TRUE))

    # drop fe from formula if required
    if(!is.null(fe)){
      formula_X <- update(formula_X, paste("~ . -", paste(fe, collapse = "+")))
    }

    # add a cluster to formula to get full model.frame
    if (!is.null(cluster)) {
      formula <- update(formula, paste("~ . +", paste(cluster, collapse = "+")))
    }

    if(sum(bootcluster %in% c(NULL, "max", "min")) == 0){
      formula <- update(formula, paste("~ . +", paste(bootcluster, collapse = "+")))
    }

    of$formula <- as.call(formula)

    o <- match(c("formula", "data", "weights"), names(of), 0L)
    of <- of[c(1L, o)]

    of[[1L]] <- quote(stats::model.frame)
    # names(of$fml) <- "formula"
    of <- eval(of, parent.frame())

    # check if one of the fixed effects or cluster variables is not of
    # type character or factor
    fixedid <- names(object$fe)[which(names(object$fe)!= fe)]

    # are fixed effects neither character nor factor?
    j <- which(!(sapply(data.frame(of[,c(fixedid)]), class) %in%  c("character", "factor")))
    # if only one fixed effect & if it is not a factor, `of[,c(fixedid)]` is not a data.frame but a vector
    if(length(j) == 1){
      of[, c(fixedid)] <- factor(of[, c(fixedid)])
    } else if(length(j) > 1) {
      of[, c(fixedid)][,j] <- lapply(j, function(x) factor(of[, c(fixedid)][,x]))
    }
    N_model <- object$N
    model_param_names <- rownames(coef(object))
  } else if (class(object) == "lm") {

    of <- object$call
    o <- match(c("formula", "data", "weights"), names(of), 0L)
    # keep only required arguments
    of <- of[c(1L, o)]
    # add argument to function
    # of$formula <- of$fml
    of$drop.unused.levels <- TRUE

    # create formula objects by adding fixed effects and clusters from call
    # add potential other cluster variables from cluster argument
    formula <- eval(of$formula)

    # formula with only depvar and covariates, needed to construct design matrix X
    formula_X <- formula


    # add cluster variables to formula
    if (!is.null(cluster)) {
      formula <- update(formula, paste("~ . +", paste(cluster, collapse = "+")))
      # formula <- update(formula, paste("~ . -",fe))
    }

    # add bootcluster variables if not in model
    if(!(is.null(bootcluster) || bootcluster == "max" || bootcluster == "min")){
      formula <- update(formula, paste("~ . +", paste(bootcluster, collapse = "+")))
    }

    of$formula <- as.call(formula)

    o <- match(c("formula", "data", "weights"), names(of), 0L)
    of <- of[c(1L, o)]

    of[[1L]] <- quote(stats::model.frame)
    of <- eval(of, parent.frame())

    N_model <- length(residuals(object))
    model_param_names <- names(coef(object))

    # fe argument not allowed with boottest.lm
    fe <- NULL
  } else if(class(object) == "ivreg"){

    of <- object$call
    o <- match(c("formula", "data", "weights"), names(of), 0L)
    # keep only required arguments
    of <- of[c(1L, o)]
    # add argument to function
    of$drop.unused.levels <- TRUE

    # create formula objects by adding fixed effects and clusters from call
    # add potential other cluster variables from cluster argument
    formula <- eval(of$formula)
    # formula with only depvar and covariates, needed to construct design matrix X
    formula_X <- formula(Formula::Formula(formula), collapse = TRUE)

    fml <- Formula::as.Formula(of$formula)
    fml_linear <- formula(fml, lhs = 1, rhs = 1)
    fml_iv <- formula(fml, lhs = 0, rhs = 2)
    fml_linear_cluster <- update(fml_linear, paste("~ . +", paste(cluster, collapse = "+")))
    fml_cluster <- Formula::as.Formula(fml_linear_cluster, fml_iv)
    fml_iv_cluster <- formula(fml_cluster, collapse = TRUE, update = TRUE)

    formula <- formula(Formula::as.Formula(fml_linear, fml_iv), collapse = TRUE, update = TRUE)

    if(sum(bootcluster %in% c(NULL, "max", "min")) == 0){
      formula <- update(formula, paste("~ . +", paste(bootcluster, collapse = "+")))
    }

    of$formula <- as.call(fml_iv_cluster)
    o <- match(c("formula", "data", "weights"), names(of), 0L)
    of <- of[c(1L, o)]
    of[[1L]] <- quote(stats::model.frame)
    of <- eval(of, parent.frame())
    N_model <- object$nobs

    model_param_names <- names(coef(object))


  }



  # ---------------------------------------------------------------------------- #
  # From here on: everything the same, independent of model class
  # Step 2: Add warning / error if cluster variables contain NAs

  N <- dim(of)[1]
  N_diff <- abs(N - N_model)

  if(na_omit == FALSE && N_diff != 0){
    stop("One or more cluster or bootcluster variables set in boottest() contain
         NA values. This is not allowed if na_omit == FALSE.
         Please either delete the missing values from your model prior
         to estimating the regression model and conducting inference with
         boottest(), or set the boottest() argument na_omit = TRUE. Note that in
         the second case, parameter estimation and
         bootstrap inference will be conducted on two different samples.")
  }
  # add a warning if missing values are deleted due to NA values in cluster variables

  if (na_omit == TRUE) {
    if (N_diff == 1) {
      warning(paste(
        N_diff,
        "observation deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."
      ),
      call. = FALSE,
      noBreaks. = TRUE
      )
    } else if (N_diff > 1) {
      warning(paste(
        N_diff,
        "observations deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."
      ),
      call. = FALSE,
      noBreaks. = TRUE
      )
    }
  # this part of the code is superfluous, right?
  } else if (na_omit == FALSE) {
    if (N_diff >= 1) {
      stop(paste(
        N_diff,
        "NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest(). If you are fine with deleting missing values, set na_omit = TRUE."
      ),
      call. = FALSE,
      noBreaks. = TRUE
      )
    }
  }


  # ---------------------------------------------------------------------------- #
  # Step 3: assign Y, X, weights, fixed_effects, W etc.


  model_frame <- of

  Y <- model.response(model_frame)
  # X: need to delete clusters
  X <- model.matrix(formula_X, model_frame)

  if (!is.null(fe) ) {
    # note: simply update(..., -1) does not work - intercept is dropped, but all levels of other fe are kept
    X <- X[, -which(colnames(X) == "(Intercept)")]
    fixed_effect <- as.data.frame(model_frame[, fe])
  } else {
    fixed_effect <- NULL
  }

  k <- dim(X)[2]


  if(class(object) == "ivreg"){
    X_exog <- X[, names(object$exogenous)]
    X_endog <- X[, names(object$endogenous)]
    instruments <- X[, names(object$instruments)]
  } else{
    X_exog <- NULL
    X_endog <- NULL
    instruments <- NULL
  }

  weights <- as.vector(model.weights(of))

  if (is.null(weights)) {
    weights <- rep(1, N)
  }

  # ---------------------------------------------------------------------------- #
  # Step 4: preprocess clusters
  # Note: a large part of the following code was taken and adapted from the
  # sandwich R package, which is distributed under GPL-2 | GPL-3
  # Zeileis A, KC6ll S, Graham N (2020). "Various Versatile Variances: An Object-Oriented
  # Implementation of Clustered Covariances in R." _Journal of Statistical Software_,
  # *95*(1), 1-36. doi: 10.18637/jss.v095.i01 (URL: https://doi.org/10.18637/jss.v095.i01).

  # changes by Alexander Fischer:
  # no essential changes, but slight reorganization of pieces of code

  # ---------------------------------------------------------------------------- #
  # Start Sandwich code
  # ---------------------------------------------------------------------------- #

  clustid <- cluster
  cluster_names <- clustid
  clustid <- as.data.frame(model_frame[, clustid], stringsAsFactors = FALSE)
  clustid_dims <- ncol(clustid)


  i <- !sapply(clustid, is.numeric)
  clustid[i] <- lapply(clustid[i], as.character)

  # taken from multiwayvcov::cluster.boot
  acc <- list()
  for (i in 1:clustid_dims) {
    acc <- append(acc, utils::combn(1:clustid_dims, i, simplify = FALSE))
  }

  vcov_sign <- sapply(acc, function(i) (-1)^(length(i) + 1))
  acc <- acc[-1:-clustid_dims]

  if (clustid_dims == 1) {
    names(clustid) <- cluster_names
  }

  if (clustid_dims > 1) {
    for (i in acc) {
      clustid <- cbind(clustid, Reduce(paste0, clustid[, i]))
      names(clustid)[length(names(clustid))] <- Reduce(paste0, names(clustid[, i]))
      # cluster_names <- cbind(cluster_names, Reduce(paste0, clustid[,i]))
    }
  }

  # ---------------------------------------------------------------------------- #
  # End Sandwich code
  # ---------------------------------------------------------------------------- #


  N_G <- sapply(clustid, function(x) length(unique(x)))

  # create a bootcluster vector
  if (length(bootcluster) == 1) {
    if (bootcluster == "max") {
      bootcluster <- clustid[which.max(N_G)]
    } else if (bootcluster == "min") {
      bootcluster <- clustid[which.min(N_G)]
    } else if (mean(bootcluster %in% c(model_param_names, cluster_names)) == 1) {
      # all bootcluster variables in model parameter names or cluster names
      # no comma - then bootcluster is a data.frame. this is required later.
      bootcluster <- clustid[which(names(clustid) == bootcluster)]
    }
  } else if (length(bootcluster) > 1) {
    # if a character vector of length > 1 is used to specify the bootcluster
    # same as above: model_frame[bootcluster] -> bootcluster will be a data.frame
    bootcluster <- as.data.frame(Reduce(paste0, model_frame[bootcluster]))
  }



  # --------------------------------------------------------------------------------------- #
  # collect output

  if(class(object) == "ivreg"){
    n_exog <- length(names(object$exogenous))
    n_endog <- length(names(object$endogenous))
    if(!is.matrix(R)){
      R0 <- rep(0, n_exog + n_endog)
      R0[match(param,c(names(object$exogenous), names(object$endogenous)))] <- R
      #R0[1:n_exog][match(param, colnames(X_exog))] <- R
      #R0[(n_exog +1):(n_exog + n_endog)][match(param, colnames(X_endog))] <- R
      names(R0) <- c(names(object$exogenous), names(object$endogenous))
    } else {
      R0 <- R
    }
  } else {
    if(!is.matrix(R)){
      R0 <- rep(0, length(colnames(X)))
      R0[match(param, colnames(X))] <- R
      names(R0) <- colnames(X)
    } else {
      R0 <- R
    }

  }


  res <- list(
    Y = Y,
    X = X,
    weights = weights,
    fixed_effect = fixed_effect,
    # W = W,
    # n_fe = n_fe,
    N = N,
    k = k,
    clustid = clustid,
    vcov_sign = vcov_sign,
    clustid_dims = clustid_dims,
    N_G = N_G,
    bootcluster = bootcluster,
    R0 = R0,
    model_frame = model_frame,
    X_exog = X_exog,
    X_endog = X_endog,
    instruments = instruments
  )

  res

}
