
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wildboottestjlr

## Installation

Julia needs to be installed.

``` r
# clone wildboottestjlr from github
# then 
# install(path_to_wildboottestjlr)
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
```

This will initiate Julia and load WildBootTest.jl.

Note: currently, only a method for `lm` is implemented. The lm method
only works for simple cases (the boottest & fwildclusterboot defaults).
Further, the data objects are not sorted, so all inference will be
incorrect.

``` r
data(voters)
lm_fit <- lm(proposition_vote ~ treatment  + log_income, data = voters)
boottest(lm_fit, clustid = "group_id1", B = 999, param = "treatment")
#> $point_estimate
#> [1] 0.08925428
#> 
#> $p_val
#> [1] 0
#> 
#> $conf_int
#>            [,1]      [,2]
#> [1,] 0.04076565 0.1371895
#> 
#> attr(,"class")
#> [1] "boottest"
```
