# library(testthat)
# library(randomForest)
library(SuperLearner)

if(all(sapply(c("testthat", "randomForest", "mlbench"), requireNamespace))){
  
testthat::context("Learner: randomForest")

############################
# Setup test dataset from mlbench.

data(BreastCancer, package="mlbench")

data = na.omit(BreastCancer)

set.seed(1)

# Reduce to a dataset of 100 observations to speed up testing.
data = data[sample(nrow(data), 100), ]

# Expand out factors into indicators.
X = data.frame(model.matrix(~ . -1, subset(data, select=-c(Class, Id))))

# Limit to 20 variables to speed up testing.
X = X[, 1:20]

Y = as.numeric(data$Class == "malignant")
print(table(Y))

###########################
# Test basic SL ensemble with RandomForest.

sl_lib = c("SL.randomForest", "SL.mean")

sl = SuperLearner(Y = Y, X = X, SL.library = sl_lib,
                  cvControl = list(V = 2),
                  family = binomial())
print(sl)
rm(sl_lib)

#############################
# test create.Learner with randomForest

######
# Test default call.
create_rf = create.Learner("SL.randomForest")
print(create_rf)
sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names,
                  cvControl = list(V = 2),
                  family = binomial())
print(sl)

# Clean up global environment.
rm(list=create_rf$names)



###########

# Create an environment to store the learners.
sl_env = new.env()

# Specify an environment and test verbose.
create_rf = create.Learner("SL.randomForest", env = sl_env, verbose=T)
print(create_rf)
print(ls(sl_env))
print(length(sl_env))

# Attach the environment with the learner functions so SL can access them.
sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names,
                  cvControl = list(V = 2),
                  family = binomial(),
                  env = sl_env)
print(sl)



############

# Create a new environment to start this test from scratch.
sl_env = new.env()

# Test a custom tune list but only specify mtry.
tune_rf = list(mtry = c(1, 2))
create_rf = create.Learner("SL.randomForest", tune = tune_rf, detailed_names = T,
                           env = sl_env)
print(create_rf)
print(ls(sl_env))

sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names,
                  cvControl = list(V = 2),
                  family = binomial(), env = sl_env)
print(sl)


############

# Create a new environment to start this test from scratch.
sl_env = new.env()

# Test with detailed_names = F.
create_rf = create.Learner("SL.randomForest", tune = tune_rf, detailed_names = F,
                           env = sl_env)
print(create_rf)
print(ls(sl_env))

sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial(),
                  cvControl = list(V = 2),
                  env = sl_env)
print(sl)

############

# Create a new environment to start this test from scratch.
sl_env = new.env()

# Test another version where we specify NULL as a string so that its incorporated into names.
tune_rf = list(mtry = c(1, 2), nodesize = "NULL", maxnodes = "NULL")
create_rf = create.Learner("SL.randomForest", tune = tune_rf, detailed_names = T,
                           env = sl_env)
print(create_rf)
print(ls(sl_env))

sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial(),
                  cvControl = list(V = 2),
                  env = sl_env)
print(sl)



############

# Create a new environment to start this test from scratch.
sl_env = new.env()

# Test maxnode specification, including one version that uses the default.
# We specify maxnodes using a list rather than vector so that 5 and 10 are not
# coerced into strings.
tune_rf = list(mtry = c(1, 2), maxnodes = list(5, 10, "NULL"))
create_rf = create.Learner("SL.randomForest", tune = tune_rf, detailed_names = T,
                           env = sl_env)
print(create_rf)

sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names,
                  family = binomial(),
                  cvControl = list(V = 2),
                  env = sl_env)
print(sl)

# We need to use <<- in order for the sl result to be saved in our parent frame (GlobalEnv)
with(sl_env, {
  sl <<- SuperLearner(Y = Y, X = X, SL.library = create_rf$names,
                      cvControl = list(V = 2),
                      family = binomial())
})
print(sl)

# Or we can do this.
sl = with(sl_env, SuperLearner(Y = Y, X = X, SL.library = create_rf$names,
                               cvControl = list(V = 2),
                               family = binomial()))
print(sl)


################
# Test multicore.

### 2018-07-10 Removing test since generating WARN NOTE on CRAN-devel - EP

# Only run in RStudio so that automated CRAN checks don't give errors.
#if (.Platform$GUI == "RStudio") {

  # Note we don't create a new sl_env here, because we are using the env from the
  # previous test.

#  doMC::registerDoMC()
#  sl = with(sl_env, mcSuperLearner(Y = Y, X = X, SL.library = create_rf$names,
#                                   cvControl = list(V = 2),
#                                   family = binomial()))
#  print(sl)
#}

  }
