# library(testthat)
library(SuperLearner)

if(all(sapply(c("testthat", "quadprog"), requireNamespace))){
  
testthat::context("Method: CC_LS")

set.seed(1234)
n <- 100
simX <- data.frame(a = rnorm(n))
simY <- 10*simX$a + rnorm(n)

testthat::test_that("Function works with/without duplicates.",{
  # should throw a warning for duplicated columns
  set.seed(1234)
  testthat::expect_warning(test_sl1 <<- SuperLearner(Y = simY, X = simX,
                          method = "method.CC_LS",
                          SL.library = c("SL.glm","SL.glm","SL.mean")))
  # second coefficient should be 0
  testthat::expect_true(test_sl1$coef[2] == 0)
  set.seed(1234)
  test_sl2 <- SuperLearner(Y = simY, X = simX,
                          method = "method.CC_LS",
                          SL.library = c("SL.glm","SL.mean"))
  # glm sould have same coefficient as test_sl1
  testthat::expect_true(test_sl1$coef[1] == test_sl2$coef[1])
})

testthat::test_that("Function works with NAs.",{
  # make a glm wrapper that inserts an NA
  SL.glm.NA <- function(Y, X, newX, family, obsWeights, ...){
    fit <- SL.glm(Y = Y, X = X, newX = newX, family = family,
                  obsWeights = obsWeights, ...)
    fit$pred[1] <- NA
    return(fit)
  }
  # should throw a warning for NA
  set.seed(1234)
  testthat::expect_warning(test_sl3 <- SuperLearner(Y = simY, X = simX,
                          method = "method.CC_LS",
                          SL.library = c("SL.glm","SL.glm.NA","SL.mean")))
  # second coefficient should be 0
  testthat::expect_true(test_sl1$coef[2] == 0)
})

  }
