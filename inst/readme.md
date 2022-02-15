# Tests in wildboottestjlr

Tests are implemented via the `tinytest` package and are triggered to run on github actions with every commit of a development version (with three-digit labels, e.g. 0.1.1). 

Note that these unit tests might run for a long time (around 30 minutes).

## External Tests: Testing wildboottestjlr against other implementations of the wild cluster bootstrap 
+ In [test_ols_fwildclusterboot.R](https://github.com/s3alfisc/wildboottestjlr/tree/main/inst/tinytest) the wild cluster bootstrap for OLS in `wildboottestjlr` is tested against the [fwildclusterboot package](). This test suit is divided in two parts: first, the bootstrap implementations are tested "stochastically" for large B. Second, the tests check if the t-stats computed in `fwildclusterboot` and `wildboottestjlr` are exactly identical. Third, a set of tests uses the fact that in the enumeration case, both bootstrap implementation use the same weights matrix and therefore, the resulting inferential parameters should be exactly identical. 

+ In [test_iv.R](), `wildboottestjlr's` IV method is tested against the Stata implementation of the wild cluster bootstrap, `boottest`. This is still to be done.

+ In [test_waldtest_deterministic.R](), we check if the non-bootstrapped F-statistics computed in `wildboottestjlr()` are identical to F-statistics calculated via the `fixest::wald()` function.

## Internal Tests: Does it matter if the bootstrap is applied to objects of type `lm`, `feols` or `felm`? 

As `fwildclusterboot` and `wildboottestjlr` share the same preprocessing, this is outsourced to the `fwildclusterboot` testing suite. 
