library(testthat)
library(glmnet)
library(cvAUC)

context("Meta methods")

# Create sample dataset for testing.
set.seed(1)
N <- 200
X <- matrix(rnorm(N*10), N, 10)
X <- as.data.frame(X)
Y <- rbinom(N, 1, plogis(.2*X[, 1] + .1*X[, 2] - .2*X[, 3] + .1*X[, 3]*X[, 4] - .2*abs(X[, 4])))
table(Y)

SL.library <- c("SL.glmnet", "SL.stepAIC", "SL.mean")

########################
# Test method.NNLS
test.NNLS <- SuperLearner(Y = Y, X = X, SL.library = SL.library, verbose = F, method = "method.NNLS", family = binomial())

# Check that predictions >= 0
expect_gte(min(test.NNLS$SL.predict), 0)
# Check that predictions <= 1
expect_lte(max(test.NNLS$SL.predict), 1)

########################
# Test method.NNLS2
test.NNLS2 <- SuperLearner(Y = Y, X = X, SL.library = SL.library, verbose = F, method = "method.NNLS2", family = binomial())

# Check that predictions >= 0
expect_gte(min(test.NNLS2$SL.predict), 0)
# Check that predictions <= 1
expect_lte(max(test.NNLS2$SL.predict), 1)

########################
# Test method.NNloglik
test.NNloglik <- SuperLearner(Y = Y, X = X, SL.library = SL.library, verbose = F, method = "method.NNloglik", family = binomial())

# Check that predictions >= 0
expect_gte(min(test.NNloglik$SL.predict), 0)
# Check that predictions <= 1
expect_lte(max(test.NNloglik$SL.predict), 1)

########################
# Test method.CC_LS
test.CC_LS <- SuperLearner(Y = Y, X = X, SL.library = SL.library, verbose = F, method = "method.CC_LS", family = binomial())

# Check that predictions >= 0
expect_gte(min(test.CC_LS$SL.predict), 0)
# Check that predictions <= 1
expect_lte(max(test.CC_LS$SL.predict), 1)

########################
# Test method.CC_nloglik
test.CC_nloglik <- SuperLearner(Y = Y, X = X, SL.library = SL.library, verbose = F, method = "method.CC_nloglik", family = binomial())

# Check that predictions >= 0
expect_gte(min(test.CC_nloglik$SL.predict), 0)
# Check that predictions <= 1
expect_lte(max(test.CC_nloglik$SL.predict), 1)

########################
# Test method.AUC
# This may generate a warning about lack of convergence.
test.AUC <- SuperLearner(Y = Y, X = X, SL.library = SL.library, verbose = F, method = "method.AUC", family = binomial())

# Check that AUC predictions >= 0
expect_gte(min(test.AUC$SL.predict), 0)
# Check that NNLS predictions <= 1
expect_lte(max(test.AUC$SL.predict), 1)
