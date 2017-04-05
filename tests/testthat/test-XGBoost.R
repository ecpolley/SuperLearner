library(testthat)
library(xgboost)
library(SuperLearner)

context("Learner: XGBoost")

# Create sample dataset for testing.
set.seed(1)
N <- 200
X <- matrix(rnorm(N * 10), N, 10)
X <- as.data.frame(X)
Y_bin <- rbinom(N, 1, plogis(.2*X[, 1] + .1*X[, 2] - .2*X[, 3] + .1*X[, 3]*X[, 4] - .2*abs(X[, 4])))
table(Y_bin)

SL.library <- c("SL.mean", "SL.xgboost")

# Test xgboost - binary classification
sl <- SuperLearner(Y = Y_bin, X = X, SL.library = SL.library,
                   cvControl = list(V = 2),
                   family = binomial())
sl

# Prediction after classification.
pred = predict(sl, X)
summary(pred$pred)

# Test xgboost - regression
Y_reg <- .2*X[, 1] + .1*X[, 2] - .2*X[, 3] + .1*X[, 3]*X[, 4] - .2*abs(X[, 4]) + rnorm(N)
summary(Y_reg)
sl <- SuperLearner(Y = Y_reg, X = X, SL.library = SL.library,
                   cvControl = list(V = 2),
                   family = gaussian())
sl

# Prediction after regression, using a dataframe.
pred = predict(sl, X)
summary(pred$pred)

# Test xgboost - multi-classification
# TODO: add test here.

test_that("Test create.SL.xgboost", {
  # Create a new environment to hold the functions.
  sl_env = new.env()
  xgb_grid = create.SL.xgboost(tune = list(ntrees = c(5, 10), max_depth = c(1, 2),
                      minobspernode = 10, shrinkage = c(0.1, 0.01, 0.001)), env = sl_env)
  xgb_grid
  xgb_functions = ls(sl_env)
  expect_equal(length(xgb_functions), 12)
  # Load the functions for use in the SuperLearner call.
  attach(sl_env)
  sl <- SuperLearner(Y = Y_reg, X = X, SL.library = c(SL.library, xgb_grid$names),
                     cvControl = list(V = 2),
                     family = gaussian())
  print(sl)
  detach(sl_env)
})
