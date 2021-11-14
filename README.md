
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
to R via the `JuliaCall` package.

It currently supports wild bootstrap inference for OLS and IV models via

-   OLS: `lm`, `fixest`, `lfe`
-   IV: `ivreg`.

In the future, IV methods for `fixest` and `lfe` will be added.

## Installation

You can install Julia by following the steps described here:
<https://julialang.org/downloads/>.

`wildboottestjlr` can be installed by running

``` r
library(devtools)
install_github("s3alfisc/wildboottestjlr")
```

## Example

To initiate Julia and load `WildBootTests.jl`, run

``` r
library(wildboottestjlr)
wildboottestjlr_setup("C:/Users/alexa/AppData/Local/Programs/Julia-1.6.3/bin")
```

`wildboottestjlr's` central function is called `boottest()`. Beyond few
minor differences, it largely mirrors the `boottest()` function from the
`fwildclusterboot` package.

``` r
# set a 'global' seed in the Julia session
set_julia_seed(rng = 12313452435)

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
summary(boot_lm)
#> boottest.lm(object = lm_fit, clustid = "group_id1", param = "treatment", 
#>     B = 999, rng = 7651427)
#>  
#>  Hypothesis: 0*treatment+1*treatment+0*treatment = 0
#>  Observations: 300
#>  Bootstr. Iter: 999
#>  Bootstr. Type: rademacher
#>  Clustering: 1-way
#>  Confidence Sets: 95%
#>  Number of Clusters: 40
#> 
#>                                      term estimate statistic p.value conf.low
#> 1 0*treatment+1*treatment+0*treatment = 0    0.089     3.743   0.001     0.04
#>   conf.high
#> 1     0.143

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
0*treatment+1*treatment+0\*treatment = 0
</td>
<td style="text-align:center;">
0.089 (0.001)
</td>
<td style="text-align:center;">
0.089 (0.002)
</td>
<td style="text-align:center;">
0.089 (0.001)
</td>
</tr>
<tr>
<td style="text-align:left;box-shadow: 0px 1px">
</td>
<td style="text-align:center;box-shadow: 0px 1px">
\[0.040, 0.143\]
</td>
<td style="text-align:center;box-shadow: 0px 1px">
\[0.040, 0.139\]
</td>
<td style="text-align:center;box-shadow: 0px 1px">
\[0.039, 0.138\]
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
run. This is currently not working, see the discussion in issue ‘\#7 IV
Implementation: Task List’.

``` r
library(ivreg)
data("SchoolingReturns", package = "ivreg")
data <- SchoolingReturns
head(data)
#>   wage education experience ethnicity smsa south age nearcollege nearcollege2
#> 1  548         7         16      afam  yes    no  29          no           no
#> 2  481        12          9     other  yes    no  27          no           no
#> 3  721        12         16     other  yes    no  34          no           no
#> 4  250        11         10     other  yes    no  27         yes          yes
#> 5  729        12         16     other  yes    no  34         yes          yes
#> 6  500        12          8     other  yes    no  26         yes          yes
#>   nearcollege4 enrolled married education66 smsa66 south66 feducation
#> 1         none       no     yes           5    yes      no       9.94
#> 2         none       no     yes          11    yes      no       8.00
#> 3         none       no     yes          12    yes      no      14.00
#> 4       public       no     yes          11    yes      no      11.00
#> 5       public       no     yes          12    yes      no       8.00
#> 6       public       no     yes          11    yes      no       9.00
#>   meducation fameducation kww  iq parents14 library14
#> 1      10.25            9  15  NA      both        no
#> 2       8.00            8  35  93      both       yes
#> 3      12.00            2  42 103      both       yes
#> 4      12.00            6  25  88      both       yes
#> 5       7.00            8  34 108      both        no
#> 6      12.00            6  38  85      both       yes
data$parents14 <- as.factor(data$parents14)

object <- ivreg(log(wage) ~ education + poly(experience, 2) + ethnicity + smsa + south + parents14 |
                  nearcollege + poly(age, 2) + ethnicity + smsa + south + parents14,
                data = data)
# wildboottestjlr:::boottest.ivreg(object = object, B = 999, param = "education", clustid = "south")
```
