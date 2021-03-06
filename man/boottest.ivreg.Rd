% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boottest_ivreg.R
\name{boottest.ivreg}
\alias{boottest.ivreg}
\title{Fast wild cluster bootstrap inference for object of class lm}
\usage{
\method{boottest}{ivreg}(
  object,
  clustid,
  param,
  B,
  bootcluster = "max",
  conf_int = NULL,
  rng = NULL,
  R = NULL,
  beta0 = 0,
  sign_level = 0.05,
  type = "rademacher",
  impose_null = TRUE,
  p_val_type = "two-tailed",
  tol = 1e-06,
  na_omit = TRUE,
  floattype = "Float32",
  fweights = FALSE,
  getauxweights = FALSE,
  t_boot = FALSE,
  turbo = FALSE,
  maxmatsize = NULL,
  bootstrapc = FALSE,
  LIML = FALSE,
  Fuller = NULL,
  kappa = NULL,
  ARubin = FALSE,
  ssc = boot_ssc(adj = TRUE, fixef.K = "none", cluster.adj = TRUE, cluster.df =
    "conventional"),
  ...
)
}
\arguments{
\item{object}{An object of class lm}

\item{clustid}{A character vector containing the names of the cluster variables}

\item{param}{A character vector of length one. The name of the regression
coefficient for which the hypothesis is to be tested}

\item{B}{Integer. The number of bootstrap iterations. When the number of clusters is low,
increasing B adds little additional runtime.}

\item{bootcluster}{A character vector. Specifies the bootstrap clustering variable or variables. If more
than one variable is specified, then bootstrapping is clustered by the intersections of
clustering implied by the listed variables. To mimic the behavior of stata's boottest command,
the default is to cluster by the intersection of all the variables specified via the \code{clustid} argument,
even though that is not necessarily recommended (see the paper by Roodman et al cited below, section 4.2).
Other options include "min", where bootstrapping is clustered by the cluster variable with the fewest clusters.}

\item{conf_int}{A logical vector. If TRUE, boottest computes confidence
intervals by p-value inversion. If FALSE, only the p-value is returned.}

\item{rng}{An integer. Controls the random number generation, which is handled via the \code{StableRNG()} function from the \code{StableRNGs} Julia package.}

\item{R}{Hypothesis Vector giving linear combinations of coefficients. Must be either NULL or a vector of the same length as \code{param}. If NULL, a vector of ones of length param.}

\item{beta0}{A numeric. Shifts the null hypothesis
H0: param = beta0 vs H1: param != beta0}

\item{sign_level}{A numeric between 0 and 1 which sets the significance level
of the inference procedure. E.g. sign_level = 0.05
returns 0.95\% confidence intervals. By default, sign_level = 0.05.}

\item{type}{character or function. The character string specifies the type
of boostrap to use: One of "rademacher", "mammen", "norm", "gamma"
and "webb". Alternatively, type can be a function(n) for drawing
wild bootstrap factors. "rademacher" by default.
For the Rademacher and Mammen distribution, if the number of replications B exceeds
the number of possible draw ombinations, 2^(#number of clusters), then \code{boottest()}
will use each possible combination once (enumeration).}

\item{impose_null}{Logical. Controls if the null hypothesis is imposed on
the bootstrap dgp or not. Null imposed \code{(WCR)} by default.
If FALSE, the null is not imposed \code{(WCU)}}

\item{p_val_type}{Character vector of length 1. Type of p-value.
By default "two-tailed". Other options include "equal-tailed", ">" and "<".}

\item{tol}{Numeric vector of length 1. The desired accuracy
(convergence tolerance) used in the root finding procedure to find the confidence interval.
Relative tolerance of 1e-6 by default.}

\item{na_omit}{Logical. If TRUE, \code{boottest()} omits rows with missing
variables in the cluster variable that have not previously been deleted
when fitting the regression object (e.g. if the cluster variable was not used
when fitting the regression model).}

\item{floattype}{Float32 by default. Other optio: Float64. Should floating point numbers in Julia be represented as 32 or 64 bit?}

\item{fweights}{Logical. FALSE by default, TRUE for frequency weights.}

\item{getauxweights}{Logical. FALSE by default. Whether to save auxilliary weight matrix (v)}

\item{t_boot}{Logical. Should bootstrapped t-statistics be returned?}

\item{turbo}{Logical scalar, FALSE by default. Whether to exploit acceleration of the LoopVectorization package: slower on first use in a session, faster after}

\item{maxmatsize}{NULL by default = no limit. Else numeric scalar to set the maximum size of auxilliary weight matrix (v), in gigabytes}

\item{bootstrapc}{Logical scalar, FALSE by default. TRUE  to request bootstrap-c instead of bootstrap-t}

\item{LIML}{Logical scalar. False by default. TRUE for LIML or Fuller LIML}

\item{Fuller}{NULL by default. Numeric scalar. Fuller LIML factor}

\item{kappa}{Null by default. fixed κ for k-class estimation}

\item{ssc}{An object of class \code{boot_ssc.type} obtained with the function \code{\link[fwildclusterboot]{boot_ssc}}. Represents how the small sample adjustments are computed. The defaults are \verb{adj = TRUE, fixef.K = "none", cluster.adj = "TRUE", cluster.df = "conventional"}.
You can find more details in the help file for \code{boot_ssc()}. The function is purposefully designed to mimic fixest's \code{\link[fixest]{ssc}} function.}

\item{...}{Further arguments passed to or from other methods.}

\item{ARubin.}{False by default. Logical scalar. TRUE for Anderson-Rubin Test.}
}
\value{
An object of class \code{boottest}

\item{p_val}{The bootstrap p-value.}
\item{conf_int}{The bootstrap confidence interval.}
\item{param}{The tested parameter.}
\item{N}{Sample size. Might differ from the regression sample size if the
cluster variables contain NA values.}
\item{B}{Number of Bootstrap Iterations.}
\item{clustid}{Names of the cluster Variables.}
\item{N_G}{Dimension of the cluster variables as used in boottest.}
\item{sign_level}{Significance level used in boottest.}
\item{type}{Distribution of the bootstrap weights.}
\item{t_stat}{The original test statistics - either imposing the null or not - with small sample correction \code{G / (G-1)}.}
\item{test_vals}{All t-statistics calculated while calculating the
confidence interval.}
\item{t_boot}{All bootstrap t-statistics.}
\item{regression}{The regression object used in boottest.}
\item{call}{Function call of boottest.}
\item{getauxweights}{The bootstrap auxiliary weights matrix v. Only returned if getauxweights = TRUE.}
\item{t_boot}{The bootstrapped t-statistics. Only returned if t_boot = TRUE.}
}
\description{
\code{boottest.ivreg} is a S3 method that allows for fast wild cluster
bootstrap inference for objects of class ivreg by  implementing
the fast wild bootstrap algorithm developed in Roodman et al., 2019
for instrumental variable models (WRE, Davidson & McKinnon, 2010)
}
\references{
Roodman et al., 2019, "Fast and wild: Bootstrap inference in
STATA using boottest", The STATA Journal.
(\url{https://journals.sagepub.com/doi/full/10.1177/1536867X19830877})

Cameron, A. Colin, Jonah B. Gelbach, and Douglas L. Miller. "Bootstrap-based improvements for inference with clustered errors." The Review of Economics and Statistics 90.3 (2008): 414-427.

MacKinnon, James G., and Matthew D. Webb. "The wild bootstrap for few (treated) clusters." The Econometrics Journal 21.2 (2018): 114-135.

MacKinnon, James. "Wild cluster bootstrap confidence intervals." L'Actualite economique 91.1-2 (2015): 11-33.

Webb, Matthew D. Reworking wild bootstrap based inference for clustered errors. No. 1315. Queen's Economics Department Working Paper, 2013.
}
