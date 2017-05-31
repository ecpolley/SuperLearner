library(testthat)
library(SuperLearner)

context("Wrapper: dbarts")

data(Boston, package = "MASS")

set.seed(1, "L'Ecuyer-CMRG")

# Sample rows to speed up example.
row_subset = sample(nrow(Boston), 30)

Y_gaus = Boston$medv[row_subset]
Y_bin = as.numeric(Boston$medv[row_subset] > 23)


# Remove outcome from covariate dataframe.
X = Boston[row_subset, -14]

# Convert to a matrix.
X_mat = model.matrix(~ ., data = X)
# Remove intercept.
X_mat = X_mat[, -1]

##########
# Try just the wrapper itself, not via SuperLearner
model = SuperLearner::SL.dbarts(Y_gaus, X, X, family = gaussian(),
                                obsWeights = rep(1, nrow(X)))
print(summary(model$fit$object))

model = SuperLearner::SL.dbarts(Y_bin, X, X, family = binomial(),
                                obsWeights = rep(1, nrow(X)))
print(summary(model$fit$object))

# Confirm matrix X also works.
model = SuperLearner::SL.dbarts(Y_gaus, X_mat, X, family = gaussian(), obsWeights = rep(1, nrow(X)))
print(summary(model$fit$object))
model = SuperLearner::SL.dbarts(Y_bin, X_mat, X, family = binomial(), obsWeights = rep(1, nrow(X)))
print(summary(model$fit$object))



##########
# SuperLearner with the wrapper.

# Gaussian version.
sl = SuperLearner(Y_gaus, X, family = gaussian(), cvControl = list(V = 2),
                  SL.library = c("SL.mean", "SL.dbarts", "SL.bartMachine"))
print(sl)

# This won't work.
#pred = predict(sl, X)
#summary(pred$pred)

# Confirm prediction on matrix version of X.
#pred2 = predict(sl, X_mat)
#expect_equal(pred$pred, pred2$pred)


# Binomial version - much worse performance using default hyperparameters.
sl = SuperLearner(Y_bin, X, family = binomial(), cvControl = list(V = 2),
                  SL.library = c("SL.mean", "SL.dbarts", "SL.bartMachine"))
print(sl)

#pred = predict(sl, X)
#summary(pred$pred)

# Confirm prediction on matrix version of X
#pred2 = predict(sl, X_mat)
#expect_equal(pred$pred, pred2$pred)

####################
# TODO: test different argument customizations.

####################
# TODO: test hyperparameter optimization.
