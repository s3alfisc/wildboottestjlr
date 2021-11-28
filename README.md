
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- [![packageversion](https://img.shields.io/badge/Package%20version-x86_64-w64-mingw32, x86_64, mingw32, x86_64, mingw32, , 4, 0.5, 2021, 03, 31, 80133, R, R version 4.0.5 (2021-03-31), Shake and Throw-orange.svg?style=flat-square)](commits/master) -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/fwildclusterboot)](https://CRAN.R-project.org/package=fwildclusterboot) -->
<!-- ![runiverse-package](https://s3alfisc.r-universe.dev/badges/fwildclusterboot) -->
<!-- [![R-CMD-check](https://github.com/s3alfisc/fwildclusterboot/workflows/R-CMD-check/badge.svg)](https://github.com/s3alfisc/fwildclusterboot/actions) -->
<!-- [![Codecov test coverage](https://codecov.io/gh/s3alfisc/fwildclusterboot/branch/master/graph/badge.svg)](https://app.codecov.io/gh/s3alfisc/fwildclusterboot?branch=master) -->
<!-- [![](http://cranlogs.r-pkg.org/badges/grand-total/fwildclusterboot?color=blue)](https://cran.r-project.org/package=fwildclusterboot) -->
<!-- [![](http://cranlogs.r-pkg.org/badges/last-month/fwildclusterboot?color=green)](https://cran.r-project.org/package=fwildclusterboot) -->
<!-- [![minimal R version](https://img.shields.io/badge/R%3E%3D-4.0.0-6666ff.svg)](https://cran.r-project.org/) -->

<!-- badges: end -->

# wildboottestjlr

`wildboottestjlr` ports the functionality of the
[WildBootTests.jl](https://github.com/droodman/WildBootTests.jl) package
to R via the `JuliaConnectoR` package.

It currently supports wild bootstrap inference for OLS and IV models via

-   OLS: `lm`, `fixest`, `lfe`
-   IV: `ivreg`.

In the future, IV methods for `fixest` and `lfe` will be added.

## Installation / Getting Started

You can install Julia by following the steps described here:
<https://julialang.org/downloads/>.

`wildboottestjlr` can be installed by running

``` r
library(devtools)
install_github("s3alfisc/wildboottestjlr")
```

## Example

`wildboottestjlr's` central function is called `boottest()`. Beyond few
minor differences, it largely mirrors the `boottest()` function from the
`fwildclusterboot` package.

``` r
# set a 'global' seed in the Julia session
library(wildboottestjlr)

set_julia_seed(rng = 12313452435)
#> <Julia object of type MersenneTwister>
#> MersenneTwister(12313452435)

data(voters)
library(fixest)
library(lfe)

# estimation via lm(), fixest::feols() or lfe::felm()
lm_fit <- lm(proposition_vote ~ treatment  + log_income, data = voters)
feols_fit <- feols(proposition_vote ~ treatment  + log_income, data = voters)
felm_fit <- felm(proposition_vote ~ treatment  + log_income, data = voters)

boot_lm <- boottest(lm_fit, clustid = "group_id1", B = 999, param = "treatment", rng = 7651427)
boot_feols <- boottest(feols_fit, clustid = "group_id1", B = 999, param = "treatment", rng = 7651427)
boot_felm <- boottest(felm_fit, clustid = "group_id1", B = 999, param = "treatment", rng = 7651427)

# summarize results via summary() method
#summary(boot_lm)

# also possible: use msummary() from modelsummary package
library(modelsummary)
msummary(list(boot_lm, boot_feols, boot_felm), 
        estimate = "{estimate} ({p.value})", 
        statistic = "[{conf.low}, {conf.high}]"
        )  
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:center;">
Model 1
</th>
<th style="text-align:center;">
Model 2
</th>
<th style="text-align:center;">
Model 3
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1\*treatment = 0
</td>
<td style="text-align:center;">
0.089 (0.004)
</td>
<td style="text-align:center;">
0.089 (0.004)
</td>
<td style="text-align:center;">
0.089 (0.004)
</td>
</tr>
<tr>
<td style="text-align:left;box-shadow: 0px 1px">
</td>
<td style="text-align:center;box-shadow: 0px 1px">
\[0.038, 0.139\]
</td>
<td style="text-align:center;box-shadow: 0px 1px">
\[0.038, 0.139\]
</td>
<td style="text-align:center;box-shadow: 0px 1px">
\[0.038, 0.139\]
</td>
</tr>
<tr>
<td style="text-align:left;">
Num.Obs.
</td>
<td style="text-align:center;">
300
</td>
<td style="text-align:center;">
300
</td>
<td style="text-align:center;">
300
</td>
</tr>
<tr>
<td style="text-align:left;">
R2
</td>
<td style="text-align:center;">
0.045
</td>
<td style="text-align:center;">
0.045
</td>
<td style="text-align:center;">
0.045
</td>
</tr>
<tr>
<td style="text-align:left;">
R2 Adj.
</td>
<td style="text-align:center;">
0.039
</td>
<td style="text-align:center;">
0.039
</td>
<td style="text-align:center;">
0.039
</td>
</tr>
<tr>
<td style="text-align:left;">
R2 Within
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
</tr>
<tr>
<td style="text-align:left;">
R2 Pseudo
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
</tr>
<tr>
<td style="text-align:left;">
AIC
</td>
<td style="text-align:center;">
-1.9
</td>
<td style="text-align:center;">
-3.9
</td>
<td style="text-align:center;">
</td>
</tr>
<tr>
<td style="text-align:left;">
BIC
</td>
<td style="text-align:center;">
12.9
</td>
<td style="text-align:center;">
7.2
</td>
<td style="text-align:center;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Log.Lik.
</td>
<td style="text-align:center;">
4.950
</td>
<td style="text-align:center;">
4.950
</td>
<td style="text-align:center;">
</td>
</tr>
</tbody>
</table>

If `boottest()` is applied based on an object of type `ivreg`, the WCE
bootstrap [Davidson & MacKinnon
(2010)](https://www.tandfonline.com/doi/abs/10.1198/jbes.2009.07221) is
run. Due to a currently undiscovered bug, this is only working for
one-instrument models, see the discussion in issue ‘\#7 IV
Implementation: Task List’.

``` r
library(ivreg)
data("SchoolingReturns", package = "ivreg")
data <- SchoolingReturns

ivreg_fit <- ivreg(log(wage) ~ education + age + ethnicity + smsa + south + parents14 |
                  nearcollege + age  + ethnicity + smsa + south + parents14,
                data = data)

boot_ivreg <- boottest(object = ivreg_fit, B = 999, param = "education", clustid = "fameducation", type = "webb")

summary(boot_ivreg)
#> boottest.ivreg(object = ivreg_fit, clustid = "fameducation", 
#>     param = "education", B = 999, type = "webb")
#>  
#>  Hypothesis: 0*education+0*education+0*education+0*education+0*education+0*education+0*education+0*education+1*education = 0
#>  Observations: 3010
#>  Bootstr. Iter: 999
#>  Bootstr. Type: webb
#>  Clustering: 3010-way
#>  Confidence Sets: 95%
#>  Number of Clusters: 9
#> 
#>              term estimate statistic p.value conf.low conf.high
#> 1 1*education = 0     0.09     2.201   0.012    0.019     0.264
```
