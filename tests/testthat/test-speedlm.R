library(testthat)
library(SuperLearner)

context("Wrapper: speedlm")

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
model = SuperLearner::SL.speedlm(Y_gaus, X, X, family = gaussian(),
                                  obsWeights = rep(1, nrow(X)))
print(model$fit$object)
print(summary(model$fit$object))

model = SuperLearner::SL.speedlm(Y_bin, X, X, family = binomial(),
                                  obsWeights = rep(1, nrow(X)))
print(summary(model$fit$object))

# Confirm matrix X also works.
model = SuperLearner::SL.speedlm(Y_gaus, X_mat, X, family = gaussian(), obsWeights = rep(1, nrow(X)))
print(summary(model$fit$object))
model = SuperLearner::SL.speedlm(Y_bin, X_mat, X, family = binomial(), obsWeights = rep(1, nrow(X)))
print(summary(model$fit$object))



##########
# SuperLearner with the wrapper.

# Gaussian version.
sl = SuperLearner(Y_gaus, X, family = gaussian(), cvControl = list(V = 2),
                  SL.library = c("SL.mean", "SL.speedlm"))
sl

pred = predict(sl, X)
summary(pred$pred)

# Confirm prediction on matrix version of X.
pred2 = predict(sl, X_mat)
expect_equal(pred$pred, pred2$pred)


# Binomial version.
sl = SuperLearner(Y_bin, X, family = binomial(), cvControl = list(V = 2),
                  SL.library = c("SL.mean", "SL.speedlm"))
sl

pred = predict(sl, X)
summary(pred$pred)

# Confirm prediction on matrix version of X
pred2 = predict(sl, X_mat)
expect_equal(pred$pred, pred2$pred)

####################
# TODO: test different argument customizations.

####################
# TODO: test hyperparameter optimization.
