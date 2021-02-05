# library(testthat)
library(SuperLearner)

if(all(sapply(c("testthat", "MASS", "kernlab"), requireNamespace))){
  
testthat::context("Learner: ksvm")

data(Boston, package = "MASS")

Y_gaus = Boston$medv
Y_bin = as.numeric(Boston$medv > 23)

# Remove outcome from covariate dataframe.
X = Boston[, -14]

# Convert to a matrix.
X_mat = model.matrix(~ ., data = X)
# Remove intercept.
X_mat = X_mat[, -1]

set.seed(1)

##########
# Try just the wrapper itself, not via SuperLearner
ksvm = SuperLearner::SL.ksvm(Y_gaus, X, X, family = gaussian())
ksvm = SuperLearner::SL.ksvm(Y_bin, X, X, family = binomial())

# Confirm matrix X also works.
ksvm = SuperLearner::SL.ksvm(Y_gaus, X_mat, X, family = gaussian())
ksvm = SuperLearner::SL.ksvm(Y_bin, X_mat, X, family = binomial())

##########
# SuperLearner with the wrapper.

# Gaussian version.
sl = SuperLearner(Y_gaus, X, family = gaussian(), cvControl = list(V = 2),
                  SL.library = c("SL.mean", "SL.ksvm"))
sl

pred = predict(sl, X)
summary(pred$pred)

# Confirm prediction on matrix version of X
pred2 = predict(sl, X_mat)
testthat::expect_equal(pred$pred, pred2$pred)


# Binomial version.
sl = SuperLearner(Y_bin, X, family = binomial(), cvControl = list(V = 2),
                  SL.library = c("SL.mean", "SL.ksvm"))
sl

pred = predict(sl, X)
summary(pred$pred)

# Confirm prediction on matrix version of X
pred2 = predict(sl, X_mat)
testthat::expect_equal(pred$pred, pred2$pred)

  }
