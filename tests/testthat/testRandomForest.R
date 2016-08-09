library(testthat)
library(randomForest)

context("Learner: randomForest")

############################
# Setup test dataset from mlbench.

data(BreastCancer, package="mlbench")

data = na.omit(BreastCancer)

set.seed(1)

# Reduce to a dataset of 100 observations to speed up testing.
data = data[sample(nrow(data), 100), ]

# Expand out factors into indicators.
X = data.frame(model.matrix(~ . -1, subset(data, select=-c(Class))))

Y = as.numeric(data$Class == "malignant")
table(Y)

###########################
# Test basic SL ensemble with RandomForest.

sl_lib = c("SL.randomForest", "SL.mean", "SL.glmnet")

sl = SuperLearner(Y = Y, X = X, SL.library = sl_lib, family = binomial())
sl
rm(sl_lib)

#############################
# test create.Learner with randomForest

######
# Test default call.
create_rf = create.Learner("SL.randomForest")
create_rf
sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial())
sl

# Clean up global environment.
do.call(rm, as.list(create_rf$names))



###########

# Create an environment to store the learners.
sl_env = new.env()

# Specify an environment and test verbose.
create_rf = create.Learner("SL.randomForest", env = sl_env, verbose=T)
create_rf
ls(sl_env)
length(sl_env)

# Attach the environment with the learner functions so SL can access them.
attach(sl_env)
sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial())
sl
detach(sl_env)



############

# Create a new environment to start this test from scratch.
sl_env = new.env()

# Test a custom tune list but only specify mtry.
tune_rf = list(mtry = c(4, 8))
create_rf = create.Learner("SL.randomForest", tune = tune_rf, detailed_names = T,
                           env = sl_env)
create_rf

attach(sl_env)
sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial())
sl
detach(sl_env)


############

# Create a new environment to start this test from scratch.
sl_env = new.env()

# Test with detailed_names = F.
create_rf = create.Learner("SL.randomForest", tune = tune_rf, detailed_names = F,
                           env = sl_env)
create_rf

attach(sl_env)
sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial())
sl
detach(sl_env)

############

# Create a new environment to start this test from scratch.
sl_env = new.env()

# Test another version where we specify NULL as a string so that its incorporated into names.
tune_rf = list(mtry = c(4, 8), nodesize = "NULL", maxnodes = "NULL")
create_rf = create.Learner("SL.randomForest", tune = tune_rf, detailed_names = T,
                           env = sl_env)
create_rf

attach(sl_env)
sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial())
sl
detach(sl_env)



############

# Create a new environment to start this test from scratch.
sl_env = new.env()

# Test maxnode specification, including one version that uses the default.
tune_rf = list(mtry = c(4, 8), maxnodes = c(5, 10, "NULL"))
create_rf = create.Learner("SL.randomForest", tune = tune_rf, detailed_names = T,
                           env = sl_env)
create_rf

attach(sl_env)
sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial())
sl
detach(sl_env)

# This cleaner version does not work unfortunately, to be investigated:
# with(sl_env, {
#   sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial())
# })




################
# Test multicore.

# Only run in RStudio so that automated CRAN checks don't give errors.
if (.Platform$GUI == "RStudio") {

  # Note we don't create a new sl_env here, because we are using the env from the
  # previous test.

  doMC::registerDoMC()
  attach(sl_env)
  sl = mcSuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial())
  sl
  detach(sl_env)
}
