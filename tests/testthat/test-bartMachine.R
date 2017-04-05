library(testthat)
library(bartMachine)

context("Learner: bartMachine")

# Create sample dataset for testing.
set.seed(1)
N <- 200
X <- matrix(rnorm(N*10), N, 10)
X <- as.data.frame(X)

# Binary outcome.
Y_bin <- rbinom(N, 1, plogis(.2*X[, 1] + .1*X[, 2] - .2*X[, 3] + .1*X[, 3]*X[, 4] - .2*abs(X[, 4])))
table(Y_bin)

# Continuous outcome.
Y_reg <- .2*X[, 1] + .1*X[, 2] - .2*X[, 3] + .1*X[, 3]*X[, 4] - .2*abs(X[, 4]) + rnorm(N)
summary(Y_reg)

SL.library <- c("SL.mean", "SL.bartMachine")

# Test bartMachine - binary classification.
sl <- SuperLearner(Y = Y_bin, X = X, SL.library = SL.library,
                   cvControl = list(V = 2),
                   family = binomial())
sl

# Test bartMachine - regression.
sl <- SuperLearner(Y = Y_reg, X = X, SL.library = SL.library,
                   cvControl = list(V = 2),
                   family = gaussian())
sl

# TODO: test prediction.
