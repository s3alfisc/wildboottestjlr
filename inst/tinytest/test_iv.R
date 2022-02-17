#' # Note: Test with Float64 for exact match
#'
#' library(ivreg)
#' library(sandwich)
#' library(lmtest)
#' pracma::tic()
#' library(wildboottestjlr)
#' pracma::toc()
#' library(tinytest)
#' library(data.table)
#'
#' pracma::tic()
#' wildboottestjlr::set_julia_seed(123)
#' pracma::toc()
#'
#' data("SchoolingReturns", package = "ivreg")
#'
#' data1 <- SchoolingReturns
#' setDT(data1)
#' data1 <- na.omit(data1)
#'
#' ivreg_fit <- ivreg(log(wage) ~ education + age + ethnicity + smsa + south + parents14 |
#'                      nearcollege + age  + ethnicity + smsa + south + parents14,
#'                    data = data1)
#' vcov1 <- sandwich::vcovCL(ivreg_fit,
#'                           cluster = ~ kww,
#'                           cadjust = TRUE,
#'                           type = "HC1")
#' vcov2 <- sandwich::vcovCL(ivreg_fit,
#'                           cluster = ~ smsa + kww,
#'                           cadjust = TRUE,
#'                           type = "HC1")
#'
#' res1 <- coeftest(ivreg_fit, vcov1)
#' res_df1 <- broom::tidy(res1)
#'
#' res2 <- coeftest(ivreg_fit, vcov2)
#' res_df2 <- broom::tidy(res2)
#'
#' pracma::tic()
#' boot_ivreg1 <- boottest(floattype = "Float64",
#'                           object = ivreg_fit,
#'                           B = 999,
#'                           param = "education",
#'                           clustid = "kww",
#'                           type = "mammen",
#'                           impose_null = TRUE)
#' pracma::toc()
#'
#'
#' expect_equivalent(boot_ivreg1$t_stat, res_df1[res_df1$term == "education",'statistic'])
#'
#'
#' boot_ivreg2 <- boottest(floattype = "Float64",
#'                           object = ivreg_fit,
#'                           B = 999,
#'                           param = "education",
#'                           clustid = c("kww", "age"),
#'                           type = "rademacher")
#'
#'
#' expect_equivalent(boot_ivreg2$t_stat, res_df1[res_df2$term == "education",'statistic'])
#'
#'
#'
#'
#'
#'
#' library(JuliaConnectoR)
#' library(ivreg)
#' library(data.table)
#'
#' startJuliaServer()
#'
#' WildBootTests <- juliaImport("WildBootTests")
#' data("SchoolingReturns", package = "ivreg")
#' # drop all NA values
#' setDT(SchoolingReturns)
#' SchoolingReturns <- na.omit(SchoolingReturns)
#'
#' ivreg_fit <- ivreg(log(wage) ~ education + age |
#'                      iq + age,
#'                    data = SchoolingReturns)
#' summary(ivreg_fit)
#' predexog <- cbind(1, SchoolingReturns$age)
#' predendog <- SchoolingReturns$education
#' inst <- as.numeric(SchoolingReturns$iq)
#'
#' R <- matrix(c(0, 0, 1), nrow = 1)
#' r <- 0
#' resp <- log(SchoolingReturns$wage)
#' clustid <- cbind(as.integer(SchoolingReturns$kww))
#'
#' test <- WildBootTests$wildboottest(R,
#'                                    r,
#'                                    resp=resp,
#'                                    predexog=predexog,
#'                                    predendog = predendog,
#'                                    inst = inst,
#'                                    clustid=clustid)
#'
#'
#' WildBootTests$teststat(test) # 9.603086
#'
#'
#' clustid <- cbind(as.integer(SchoolingReturns$kww),
#'                  sample(1:10, length(SchoolingReturns$kww), TRUE))
#'
#' bootcluster <- clustid[,1]
#'
#' # only bootstrapping cluster: in bootcluster and not in clustid
#' c1 <- bootcluster_n[which(!(bootcluster_n %in% clustid))]
#' # both bootstrapping and error cluster: all variables in clustid that are also in bootcluster
#' c2 <- clustid[which(clustid %in% bootcluster_n)]
#' # only error cluster: variables in clustid not in c1, c2
#' c3 <- clustid[which(!(clustid %in% c(c1, c2)))]
#' all_c <- c(c1, c2, c3)
#'
#'
#' test <- WildBootTests$wildboottest(R,
#'                                    r,
#'                                    resp=resp,
#'                                    predexog=predexog,
#'                                    predendog = predendog,
#'                                    inst = inst,
#'                                    clustid=clustid)
#' #' Error: Evaluation in Julia failed.
#' #' Original Julia error message:
#' #'   UndefRefError: access to undefined reference
#' #' Stacktrace:
#' #'   [1] getproperty
#' #' @ .\Base.jl:42 [inlined]
#' #' [2] Init!(o::WildBootTests.StrBootTest{Float32})
#' #' @ WildBootTests C:\Users\alexa\.julia\packages\WildBootTests\J2W2H\src\init.jl:167
#' #' [3] boottestWRE!(o::WildBootTests.StrBootTest{Float32})
#' #' @ WildBootTests C:\Users\alexa\.julia\packages\WildBootTests\J2W2H\src\WildBootTests.jl:48
#' #' [4] plot!(o::WildBootTests.StrBootTest{Float32})
#' #' @ WildBootTests C:\Users\alexa\.julia\packages\WildBootTests\J2W2H\src\plot-CI.jl:48
#' #' [5] __wildboottest(R::Matrix{Float32}, r::Vector{Float32}; resp::Vector{Float32}, predexog::Matrix{Float32}, predendog::Matrix{Float32}, inst::Matrix{Float32}, R1::Matrix{Float32}, r1::Vector{Float32}, clustid::Matrix{Int64}, nbootclustvar::Int8, nerrclustvar::Int8, issorted::Bool, hetrobust::Bool, feid::Vector{Int64}, fedfadj::Bool, obswt::Vector{Float32}, fweights::Bool, maxmatsize::Float16,
#'


