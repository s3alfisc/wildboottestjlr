# Note: Test with Float64 for exact match

library(ivreg)
library(sandwich)
library(lmtest)
library(wildboottestjlr)

data("SchoolingReturns", package = "ivreg")
data1 <- SchoolingReturns
ivreg_fit <- ivreg(log(wage) ~ education + age + ethnicity + smsa + south + parents14 |
                     nearcollege + age  + ethnicity + smsa + south + parents14,
                   data = data1)
vcov1 <- sandwich::vcovCL(ivreg_fit,
                         cluster = ~ smsa,
                         cadjust = TRUE,
                         type = "HC1")
vcov2 <- sandwich::vcovCL(ivreg_fit,
                         cluster = ~ smsa + south,
                         cadjust = TRUE,
                         type = "HC1")

res1 <- coeftest(ivreg_fit, vcov1)
res_df1 <- broom::tidy(res1)

res2 <- coeftest(ivreg_fit, vcov2)
res_df2 <- broom::tidy(res2)


boot_ivreg1 <- boottest(floattype = "Float64",
                       object = ivreg_fit,
                       B = 999,
                       param = x,
                       clustid = "smsa",
                       type = "rademacher")
expect_equivalent(boot_ivreg1$t_stat, res_df1[res_df$term == x,'statistic'], tol = 0.02)


skip <- TRUE
if(skip){
  boot_ivreg2 <- boottest(floattype = "Float64",
                          object = ivreg_fit,
                          B = 999,
                          param = x,
                          clustid = c("smsa", "south"),
                          type = "rademacher")
  expect_equivalent(boot_ivreg2$t_stat, res_df2[res_df$term == x,'statistic'], tol = 0.02)
}


