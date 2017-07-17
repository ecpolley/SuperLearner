library(testthat)
library(glmnet)
library(cvAUC)

context("Meta methods")

# Create sample dataset for testing.
set.seed(1)
N <- 200
X <- matrix(rnorm(N * 6), N, 6)
X <- as.data.frame(X)
Y <- rbinom(N, 1, plogis(.2*X[, 1] + .1*X[, 2] + 2*X[, 3] + .1*X[, 3]*X[, 4] - .2*abs(X[, 4])))
table(Y)

SL.library <- c("SL.rpart", "SL.glmnet", "SL.mean")

########################
# Test method.NNLS
test.NNLS <- SuperLearner(Y = Y, X = X, SL.library = SL.library, verbose = F,
                          cvControl = list(V = 2),
                          method = "method.NNLS", family = binomial())
print(test.NNLS)

# Check that predictions >= 0
expect_gte(min(test.NNLS$SL.predict), 0)
# Check that predictions <= 1
expect_lte(max(test.NNLS$SL.predict), 1)

# Test explicit prediction
pred = predict(test.NNLS)
summary(pred$pred)

# Check that predictions >= 0
expect_gte(min(pred$pred), 0)
# Check that predictions <= 1
expect_lte(max(pred$pred), 1)

##########
# Test with a failing algorithm.

SL.bad_algorithm = function(Y, X, newX, ...) { stop("bad algorithm") }

sl_bad <- SuperLearner(Y = Y, X = X, verbose = T,
                       SL.library = c(SL.library, "SL.bad_algorithm"),
                       cvControl = list(V = 2),
                       method = "method.NNLS", family = binomial())
print(sl_bad)

# Check that predictions >= 0
expect_gte(min(sl_bad$SL.predict), 0)
# Check that predictions <= 1
expect_lte(max(sl_bad$SL.predict), 1)

# Test explicit prediction
pred = predict(sl_bad)
summary(pred$pred)

# Check that predictions >= 0
expect_gte(min(pred$pred), 0)
# Check that predictions <= 1
expect_lte(max(pred$pred), 1)

# Need to cleanup so future tests don't fail but then use this object.
rm(sl_bad)

########################
# Test method.NNLS2
# This will give an error in quadprog if two learners have the same predictions.
test.NNLS2 <- SuperLearner(Y = Y, X = X, SL.library = SL.library, verbose = F,
                           cvControl = list(V = 2),
                           method = "method.NNLS2", family = binomial())
print(test.NNLS2)

# Check that predictions >= 0
expect_gte(min(test.NNLS2$SL.predict), 0)
# Check that predictions <= 1
expect_lte(max(test.NNLS2$SL.predict), 1)

######
# Test with a failed algorithm.

# TODO: fix method.NNLS2 for failed algorithms. Not clear how to fix this.
if (F) {
  sl_bad <- SuperLearner(Y = Y, X = X, verbose = T,
                         SL.library = c(SL.library, "SL.bad_algorithm"),
                         cvControl = list(V = 2),
                         method = "method.NNLS2", family = binomial())
  print(sl_bad)

  # Check that predictions >= 0
  expect_gte(min(sl_bad$SL.predict), 0)
  # Check that predictions <= 1
  expect_lte(max(sl_bad$SL.predict), 1)

  # Test explicit prediction
  pred = predict(sl_bad)
  summary(pred$pred)

  # Check that predictions >= 0
  expect_gte(min(pred$pred), 0)
  # Check that predictions <= 1
  expect_lte(max(pred$pred), 1)

  # Need to cleanup so future tests don't fail but then use this object.
  rm(sl_bad)
}

########################
# Test method.NNloglik
test.NNloglik <- SuperLearner(Y = Y, X = X, SL.library = SL.library, verbose = F,
                              cvControl = list(V = 2),
                              method = "method.NNloglik", family = binomial())
print(test.NNloglik)

# Check that predictions >= 0
expect_gte(min(test.NNloglik$SL.predict), 0)
# Check that predictions <= 1
expect_lte(max(test.NNloglik$SL.predict), 1)

######
# Test with a failed algorithm.

sl_bad <- SuperLearner(Y = Y, X = X, verbose = T,
                       SL.library = c(SL.library, "SL.bad_algorithm"),
                       cvControl = list(V = 2),
                       method = "method.NNloglik", family = binomial())
print(sl_bad)

summary(sl_bad$SL.predict)

# Check that predictions >= 0
expect_gte(min(sl_bad$SL.predict), 0)
# Check that predictions <= 1
expect_lte(max(sl_bad$SL.predict), 1)

# Test explicit prediction
pred = predict(sl_bad)
summary(pred$pred)

# Check that predictions >= 0
expect_gte(min(pred$pred), 0)
# Check that predictions <= 1
expect_lte(max(pred$pred), 1)

# Need to cleanup so future tests don't fail but then use this object.
rm(sl_bad)

########################
# Test method.CC_LS
test.CC_LS <- SuperLearner(Y = Y, X = X, SL.library = SL.library, verbose = F,
                           cvControl = list(V = 2),
                           method = "method.CC_LS", family = binomial())
print(test.CC_LS)

# Check that predictions >= 0
expect_gte(min(test.CC_LS$SL.predict), 0)
# Check that predictions <= 1
expect_lte(max(test.CC_LS$SL.predict), 1)


######
# Test with a failed algorithm.

# TODO: fix CC_LS for bad algorithms. Not clear how to do so.
if (F) {
  sl_bad <- SuperLearner(Y = Y, X = X, verbose = T,
                         SL.library = c(SL.library, "SL.bad_algorithm"),
                         cvControl = list(V = 2),
                         method = "method.CC_LS", family = binomial())
  print(sl_bad)

  summary(sl_bad$SL.predict)

  # Check that predictions >= 0
  expect_gte(min(sl_bad$SL.predict), 0)
  # Check that predictions <= 1
  expect_lte(max(sl_bad$SL.predict), 1)

  # Test explicit prediction
  pred = predict(sl_bad)
  summary(pred$pred)

  # Check that predictions >= 0
  expect_gte(min(pred$pred), 0)
  # Check that predictions <= 1
  expect_lte(max(pred$pred), 1)

  # Need to cleanup so future tests don't fail but then use this object.
  rm(sl_bad)
}

########################
# Test method.CC_nloglik
test.CC_nloglik <- SuperLearner(Y = Y, X = X, SL.library = SL.library, verbose = F,
                                cvControl = list(V = 2),
                                method = "method.CC_nloglik", family = binomial())
print(test.CC_nloglik)

# Check that predictions >= 0
expect_gte(min(test.CC_nloglik$SL.predict), 0)
# Check that predictions <= 1
expect_lte(max(test.CC_nloglik$SL.predict), 1)

######
# Test with a failed algorithm.

sl_bad <- SuperLearner(Y = Y, X = X, verbose = T,
                       SL.library = c(SL.library, "SL.bad_algorithm"),
                       cvControl = list(V = 2),
                       method = "method.CC_nloglik", family = binomial())
print(sl_bad)

summary(sl_bad$SL.predict)

# Check that predictions >= 0
expect_gte(min(sl_bad$SL.predict), 0)
# Check that predictions <= 1
expect_lte(max(sl_bad$SL.predict), 1)

# Test explicit prediction
pred = predict(sl_bad)
summary(pred$pred)

# Check that predictions >= 0
expect_gte(min(pred$pred), 0)
# Check that predictions <= 1
expect_lte(max(pred$pred), 1)

# Need to cleanup so future tests don't fail but then use this object.
rm(sl_bad)

########################
# Test method.AUC
# This may generate a warning about lack of convergence.
test.AUC <- SuperLearner(Y = Y, X = X, SL.library = SL.library, verbose = F,
                         cvControl = list(V = 2),
                         method = "method.AUC", family = binomial())
print(test.AUC)

# Check that AUC predictions >= 0
expect_gte(min(test.AUC$SL.predict), 0)
# Check that NNLS predictions <= 1
expect_lte(max(test.AUC$SL.predict), 1)

######
# Test with a failed algorithm.

sl_bad <- SuperLearner(Y = Y, X = X, verbose = T,
                       SL.library = c(SL.library, "SL.bad_algorithm"),
                       cvControl = list(V = 2),
                       method = "method.AUC", family = binomial())
print(sl_bad)

summary(sl_bad$SL.predict)

# Check that predictions >= 0
expect_gte(min(sl_bad$SL.predict), 0)
# Check that predictions <= 1
expect_lte(max(sl_bad$SL.predict), 1)

# Test explicit prediction
pred = predict(sl_bad)
summary(pred$pred)

# Check that predictions >= 0
expect_gte(min(pred$pred), 0)
# Check that predictions <= 1
expect_lte(max(pred$pred), 1)

# Need to cleanup so future tests don't fail but then use this object.
rm(sl_bad)
