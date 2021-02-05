#library(testthat)
library(SuperLearner)

if(all(sapply(c("testthat", "MASS"), requireNamespace))){
  
testthat::context("Wrapper: glm")

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
model = SuperLearner::SL.glm(Y_gaus, X, X, family = gaussian(),
                                  obsWeights = rep(1, nrow(X)))
print(model$fit$object)
print(summary(model$fit$object))

model = SuperLearner::SL.glm(Y_bin, X, X, family = binomial(), model = FALSE,
                                  obsWeights = rep(1, nrow(X)))
print(summary(model$fit$object))


# Display element names in the model fit object.
print(names(model$fit$object))

# Confirm that we are conserving memory.
testthat::test_that("Memory usage: fit obj does not contain the model element.", {
          testthat::expect(!"model" %in% names(model$fit$object),
                 "'model' does exist in the fit object, but shouldn't.")
  })

# Confirm that not conserving memory also works.
testthat::test_that("Memory usage: fit obj does contain the model element.", {
          model = SuperLearner::SL.glm(Y_bin, X, X, family = binomial(),
                                  obsWeights = rep(1, nrow(X)),
                                  model = T)
          testthat::expect("model" %in% names(model$fit$object),
                 "'model' should exist in the fit object.")
  })

# Confirm matrix X also works.
model = SuperLearner::SL.glm(Y_gaus, X_mat, X, family = gaussian(), obsWeights = rep(1, nrow(X)))
print(summary(model$fit$object))
model = SuperLearner::SL.glm(Y_bin, X_mat, X, family = binomial(), obsWeights = rep(1, nrow(X)))
print(summary(model$fit$object))



##########
# SuperLearner with the wrapper.

# Gaussian version.
sl = SuperLearner(Y_gaus, X, family = gaussian(),
                  SL.library = c("SL.mean", "SL.glm"))
print(sl)

pred = predict(sl, X)
print(summary(pred$pred))

# Confirm prediction on matrix version of X.
pred2 = predict(sl, X_mat)
testthat::expect_equal(pred$pred, pred2$pred)


# Binomial version.
sl = SuperLearner(Y_bin, X, family = binomial(),
                  SL.library = c("SL.mean", "SL.glm"))
print(sl)

pred = predict(sl, X)
# These predictions should be in [0, 1].
print(summary(pred$pred))

# Confirm prediction on matrix version of X
pred2 = predict(sl, X_mat)
testthat::expect_equal(pred$pred, pred2$pred)

####################
# TODO: test different argument customizations.

####################
# TODO: test hyperparameter optimization.

  }
