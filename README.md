
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wildboottestjlr

## Installation

Julia needs to be installed.

``` r
# clone wildboottestjlr from github
# then 
# devtools::install(path_to_wildboottestjlr)
```

## Example

To initiate the package, run

``` r
library(wildboottestjlr)
wildboottestjlr_setup("C:/Users/alexa/AppData/Local/Programs/Julia-1.6.3/bin")
#> Julia version 1.6.3 at location C:\Users\alexa\AppData\Local\Programs\Julia-1.6.3\bin will be used.
#> Loading setup script for JuliaCall...
#> Finish loading setup script for JuliaCall.
#> Warning: The internal function knitr:::wrap() has been deprecated. Please use
#> the exported function knitr::sew() instead.

#> Warning: The internal function knitr:::wrap() has been deprecated. Please use
#> the exported function knitr::sew() instead.
# if WildBootTest.jl is not installed, run 
#wildboottestjlr_setup("C:/Users/alexa/AppData/Local/Program#s/Julia-1.6.3/bin", install_WildBooTestjl = TRUE)
```

This will initiate Julia and load WildBootTest.jl.
`wildboottestjlr_setup()` will have to run once for each r session.

The `set_julia_seed()` function allows to set a “global” seed within the
Julia session:

``` r
set_julia_seed(rng = 12313452435)
```

Note: currently, only a method for `lm` is implemented. The
`boottest.lm()` method only works for simple cases (one-way clustering,
not all of boottest’s functionality is currently supported).

``` r
data(voters)
lm_fit <- lm(proposition_vote ~ treatment  + log_income, data = voters)
boottest(lm_fit, clustid = "group_id1", B = 999, param = "treatment")
#> $point_estimate
#> [1] 0.08925428
#> 
#> $p_val
#> [1] 0.002002002
#> 
#> $conf_int
#>           [,1]      [,2]
#> [1,] 0.0414222 0.1342879
#> 
#> attr(,"class")
#> [1] "boottest"
```

How does it compare to ´fwildclusterboot´ ?

``` r
library(fwildclusterboot)
#> Registered S3 methods overwritten by 'fwildclusterboot':
#>   method           from           
#>   glance.boottest  wildboottestjlr
#>   plot.boottest    wildboottestjlr
#>   summary.boottest wildboottestjlr
#>   tidy.boottest    wildboottestjlr
#> 
#> Attaching package: 'fwildclusterboot'
#> The following object is masked from 'package:wildboottestjlr':
#> 
#>     boottest
res <- fwildclusterboot::boottest(lm_fit, clustid = "group_id1", B = 999, param = "treatment")
summary(res)
#> boottest.lm(object = lm_fit, clustid = "group_id1", param = "treatment", 
#>     B = 999)
#>  
#>  Hypothesis: 1*treatment = 0
#>  Observations: 300
#>  Bootstr. Iter: 999
#>  Bootstr. Type: rademacher
#>  Clustering: 1-way
#>  Confidence Sets: 95%
#>  Number of Clusters: 40
#> 
#>              term estimate statistic p.value conf.low conf.high
#> 1 1*treatment = 0    0.089     3.756   0.001     0.04     0.139
```

### Tests

Tests that compare `wildboottestjlr` to `fwildclusterboot` can be found
in the “inst” folder.

### Debug

Code to debug `wildbottestjlr::boottest`. If you set all these
variables, you can run all code inside the function line by line:

``` r
library(wildboottestjlr)
wildboottestjlr_setup("C:/Users/alexa/AppData/Local/Programs/Julia-1.6.3/bin")
#> Warning: The internal function knitr:::wrap() has been deprecated. Please use
#> the exported function knitr::sew() instead.

#> Warning: The internal function knitr:::wrap() has been deprecated. Please use
#> the exported function knitr::sew() instead.
set_julia_seed(rng = 12313452435)
data(voters)
lm_fit <- lm(proposition_vote ~ treatment  + log_income + Q1_immigration, data = voters)
boottest(lm_fit, clustid = "group_id1", B = 999, param = "treatment")

object <- lm_fit
clustid <- "group_id1"
param <- "treatment"
B <- 999
bootcluster = c("group_id1", "Q1_immigration")
conf_int = NULL
rng = NULL
R = NULL
beta0 = 0
sign_level = NULL
type = "rademacher"
impose_null = TRUE
p_val_type = "two-tailed"
tol = 1e-6
maxiter = 10
na_omit = TRUE

devtools::load_all("C:/Users/alexa/Dropbox/wildboottestjlr/R")
#> i Loading wildboottestjlr
```
