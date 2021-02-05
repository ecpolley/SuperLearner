# library(testthat)
library(SuperLearner)

if(all(sapply(c("testthat", "nloptr"), requireNamespace))){
  
testthat::context("Method: CC_nloglik")

set.seed(1234)
n <- 100
simX <- data.frame(a = rnorm(n))
simY <- rbinom(n, 1, plogis(simX$a))

testthat::test_that("Function works with/without duplicates.",{
  # should throw a warning for duplicated columns
  set.seed(1234)
  testthat::expect_warning(test_sl1 <- SuperLearner(Y = simY, X = simX,
                          family = binomial(),
                          method = "method.CC_nloglik",
                          SL.library = c("SL.glm","SL.glm","SL.mean")))
  # second coefficient should be 0
  testthat::expect_true(test_sl1$coef[2] == 0)
  set.seed(1234)
  test_sl2 <- SuperLearner(Y = simY, X = simX,
                           family = binomial(),
                          method = "method.CC_nloglik",
                          SL.library = c("SL.glm","SL.mean"))
  # glm sould have same coefficient as test_sl1
  testthat::expect_true(test_sl1$coef[1] == test_sl2$coef[1])
})
}
