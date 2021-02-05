# library(testthat)
library(SuperLearner)

if(all(sapply(c("testthat", "glmnet", "mlbench"), requireNamespace))){
  
testthat::context("Learner: glmnet")

# Load a test dataset.
data(PimaIndiansDiabetes2, package = "mlbench")

data = PimaIndiansDiabetes2

# Omit observations with missing data.
data = na.omit(data)

Y = as.numeric(data$diabetes == "pos")
X = subset(data, select = -diabetes)

set.seed(1, "L'Ecuyer-CMRG")

#####################
# Check wrapper fit, prediction, and basic SuperLearner.
####

# Try just the wrapper itself, not via SuperLearner
glmnet = SuperLearner::SL.glmnet(Y, X, X, family = binomial(), obsWeights = rep(1, nrow(X)), id = NULL)

# Check prediction.
pred = predict(glmnet$fit, X)
summary(pred)

# Try SuperLearner with the wrapper.
sl = SuperLearner(Y, X, family = binomial(),
                  cvControl = list(V = 2),
                  SL.library = c("SL.mean", "SL.glm", "SL.glmnet"))
sl

#####################
# Check non-default hyperparameters.
####

# Change alpha.
glmnet = SuperLearner::SL.glmnet(Y, X, X, family = binomial(), alpha = 0, obsWeights = rep(1, nrow(X)), id = NULL)
glmnet = SuperLearner::SL.glmnet(Y, X, X, family = gaussian(), alpha = 0, obsWeights = rep(1, nrow(X)), id = NULL)

# Change useMin.
glmnet = SuperLearner::SL.glmnet(Y, X, X, family = binomial(), useMin = F, obsWeights = rep(1, nrow(X)), id = NULL)
glmnet = SuperLearner::SL.glmnet(Y, X, X, family = gaussian(), useMin = F, obsWeights = rep(1, nrow(X)), id = NULL)

# Change nfolds.
glmnet = SuperLearner::SL.glmnet(Y, X, X, family = binomial(), nfolds = 3, obsWeights = rep(1, nrow(X)), id = NULL)
glmnet = SuperLearner::SL.glmnet(Y, X, X, family = gaussian(), nfolds = 3, obsWeights = rep(1, nrow(X)), id = NULL)

# Change loss function.
glmnet = SuperLearner::SL.glmnet(Y, X, X, family = binomial(), loss = "auc", obsWeights = rep(1, nrow(X)), id = NULL)
glmnet = SuperLearner::SL.glmnet(Y, X, X, family = gaussian(), loss = "mae", obsWeights = rep(1, nrow(X)), id = NULL)

#####################
# Check prediction options
####

newdata = X

glmnet = SuperLearner::SL.glmnet(Y, X, X, family = binomial(), obsWeights = rep(1, nrow(X)), id = NULL)

# Test adding an extra column, which will generate a warning.
pred = predict(glmnet$fit, cbind(newdata, extra_column = 5))
summary(pred)

# See what happens when we don't remove the extra column.
tryCatch({
  pred = predict(glmnet$fit, cbind(newdata, extra_column = 5),
                          remove_extra_cols = F)
}, error = function(e) {
  cat("Got an error, as expected.\n")
  print(e)
})
summary(pred)

# Test removing a column, which will generate a warning.
pred = predict(glmnet$fit, newdata[, -5])
summary(pred)

# See what happens when we don't fill in the extra column.
tryCatch({
  pred = predict(glmnet$fit, newdata[, -5],
                 add_missing_cols = F)
}, error = function(e) {
  cat("Got an error, as expected.\n")
  print(e)
})
summary(pred)

  }
