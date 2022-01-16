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
vcov <- sandwich::vcovCL(ivreg_fit,
                         cluster = ~ nearcollege,
                         cadjust = TRUE,
                         type = "HC1")
res <- coeftest(ivreg_fit, vcov)
res_statistics <- broom::tidy(res)$statistic


boot_ivreg <- boottest(floattype = "Float64",
                       object = ivreg_fit,
                       B = 999,
                       param = "education",
                       clustid = "nearcollege",
                       type = "rademacher")
boot_ivreg$t_stat
res_statistics[2]

