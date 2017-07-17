library(testthat)
library(extraTrees)
library(SuperLearner)

context("Learner: extraTrees")

############################
# Setup test dataset.

data(Boston, package = "MASS")

set.seed(1)

# Sample 100 random observations to speed up testing.
Boston = Boston[sample(nrow(Boston), 100), ]

Y_gaus = Boston$medv
Y_bin = as.numeric(Boston$medv > 23)

# Remove outcome from covariate dataframe.
X = Boston[, -14]

# Convert to a matrix.
X_mat = model.matrix(~ ., data = X)
# Remove intercept.
X_mat = X_mat[, -1]

#####################
# Check wrapper fit, prediction, and basic SuperLearner.
####

##########
# Try just the wrapper itself, not via SuperLearner
model = SuperLearner::SL.extraTrees(Y_gaus, X, X, family = gaussian(),
                                    obsWeights = rep(1, nrow(X)))
print(model$fit$object)

model = SuperLearner::SL.extraTrees(Y_bin, X, X, family = binomial(), model = FALSE,
                                    obsWeights = rep(1, nrow(X)))
print(model$fit$object)

sl = SuperLearner::SL.extraTrees(Y_bin, X, X, family = binomial(),
                                 obsWeights = rep(1, nrow(X)))

# Check prediction.
pred = predict(sl$fit, X)
summary(pred)

sl_lib = c("SL.extraTrees", "SL.ranger", "SL.mean")

sl = SuperLearner(Y = Y_bin, X = X, SL.library = sl_lib,
                  cvControl = list(V = 2),
                  family = binomial())
print(sl)
rm(sl_lib)

#############################
# test create.Learner

######
# Test default call.
lib = create.Learner("SL.extraTrees")
print(lib)
sl = SuperLearner(Y = Y_bin, X = X, SL.library = c("SL.mean", lib$names),
                  cvControl = list(V = 2), family = binomial())
print(sl)

# Clean up global environment.
rm(list = lib$names)

###########

# Create an environment to store the learners.
sl_env = new.env()

# Specify an environment and test verbose.
lib = create.Learner("SL.extraTrees", env = sl_env, verbose = TRUE)
print(lib)
print(ls(sl_env))
print(length(sl_env))

# Attach the environment with the learner functions so SL can access them.
sl = SuperLearner(Y = Y_bin, X = X, SL.library = c("SL.mean", lib$names),
                  cvControl = list(V = 2), family = binomial(), env = sl_env)
print(sl)

############

# Create a new environment to start this test from scratch.
sl_env = new.env()

# Test a custom tune list but only specify mtry.
tune = list(mtry = c(1, 2))
lib = create.Learner("SL.extraTrees", tune = tune, detailed_names = T,
                     env = sl_env)
print(lib)
print(ls(sl_env))

sl = SuperLearner(Y = Y_bin, X = X, SL.library = c("SL.mean", lib$names),
                  cvControl = list(V = 2), family = binomial(), env = sl_env)
print(sl)

############

# Create a new environment to start this test from scratch.
sl_env = new.env()

# Test with detailed_names = F.
lib = create.Learner("SL.extraTrees", tune = tune, detailed_names = FALSE,
                     env = sl_env)
print(lib)
print(ls(sl_env))

sl = SuperLearner(Y = Y_bin, X = X, SL.library = c("SL.mean", lib$names),
                  family = binomial(), cvControl = list(V = 2), env = sl_env)
print(sl)
