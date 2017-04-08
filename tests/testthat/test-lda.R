library(testthat)
library(SuperLearner)

context("Wrapper: LDA")

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
model = SuperLearner::SL.lda(Y_bin, X, X, family = binomial())
print(model$fit$object)
summary(model$pred)

# Confirm matrix X also works.
model = SuperLearner::SL.lda(Y_bin, X_mat, X, family = binomial())
print(model$fit$object)

######
# Check prediction.

# Predict raw object.
pred = predict(model$fit$object, X)
summary(pred$posterior)

# Predict with SL wrapper object.
pred = predict(model$fit, X)
summary(pred)

##########
# SuperLearner with the wrapper.

# Binomial version.
sl = SuperLearner(Y_bin, X, family = binomial(), cvControl = list(V = 2),
                  SL.library = c("SL.mean", "SL.lda"))
sl

pred = predict(sl, X)
summary(pred$pred)

# Confirm prediction on matrix version of X
pred2 = predict(sl, X_mat)
expect_equal(pred$pred, pred2$pred)

####################
# Test different argument customizations.

# Test methods - mve gives an error.
lda_methods = create.Learner("SL.lda", detailed_names = T,
                             tune = list(method = c("mle", "moment", "t")))

sl = SuperLearner(Y_bin, X, family = binomial(), cvControl = list(V = 2),
                  SL.library = c("SL.mean", lda_methods$names))
sl

####################
# TODO: test hyperparameter optimization.


