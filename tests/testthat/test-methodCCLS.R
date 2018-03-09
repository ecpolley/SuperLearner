library(testthat)
library(SuperLearner)

context("Method: CC_LS")

set.seed(1234)
n <- 100
simX <- data.frame(a = rnorm(n))
simY <- 10*simX$a + rnorm(n)

test_that("Function works with/without duplicates.",{
  # should throw a warning for duplicated columns
  set.seed(1234)
  expect_warning(test_sl1 <<- SuperLearner(Y = simY, X = simX,
                          method = "method.CC_LS",
                          SL.library = c("SL.glm","SL.glm","SL.mean")))
  # second coefficient should be 0
  expect_true(test_sl1$coef[2] == 0)
  set.seed(1234)
  test_sl2 <- SuperLearner(Y = simY, X = simX,
                          method = "method.CC_LS",
                          SL.library = c("SL.glm","SL.mean"))
  # glm sould have same coefficient as test_sl1
  expect_true(test_sl1$coef[1] == test_sl2$coef[1])
})

test_that("Function works with NAs.",{
  # make a glm wrapper that inserts an NA
  SL.glm.NA <- function(Y, X, newX, family, obsWeights, ...){
    fit <- SL.glm(Y = Y, X = X, newX = newX, family = family,
                  obsWeights = obsWeights, ...)
    fit$pred[1] <- NA
    return(fit)
  }
  # should throw a warning for NA
  set.seed(1234)
  expect_warning(test_sl3 <- SuperLearner(Y = simY, X = simX,
                          method = "method.CC_LS",
                          SL.library = c("SL.glm","SL.glm.NA","SL.mean")))
  # second coefficient should be 0
  expect_true(test_sl1$coef[2] == 0)
})
