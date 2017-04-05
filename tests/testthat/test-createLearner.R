library(testthat)

context("create.Learner()")

############################
# Setup test dataset from mlbench.

data(BreastCancer, package="mlbench")

data = na.omit(BreastCancer)

set.seed(1)

# Reduce to a dataset of 100 observations to speed up testing.
data = data[sample(nrow(data), 100), ]

# Expand out factors into indicators.
X = data.frame(model.matrix(~ . -1, subset(data, select=-c(Id, Class))))

# Reduce number of covariates to speed up testing.
X = X[, 1:20]

Y = as.numeric(data$Class == "malignant")
print(table(Y))

###########################
# Begin tests.
###########################


########################
# Create a randomForest learner with ntree set to 100 rather than the default of 500.
create_rf = create.Learner("SL.randomForest", list(ntree = 100))
print(create_rf)
print(ls())
sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names,
                  cvControl = list(V = 2),
                  family = binomial())
print(sl)

# Clean up global environment.
rm(list=create_rf$names)



########################
# Create a randomForest learner that optimizes over mtry
create_rf = create.Learner("SL.randomForest",
                         tune = list(mtry = round(c(1, sqrt(ncol(X)), ncol(X)))))
print(create_rf)
sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names,
                  cvControl = list(V = 2),
                  family = binomial())
print(sl)

# Clean up global environment.
rm(list = create_rf$names)


########################
# Optimize elastic net over alpha, with a custom environment and detailed names.

learners = new.env()
create_enet = create.Learner("SL.glmnet", env = learners, detailed_names = T,
                                  tune = list(alpha = seq(0, 1, length.out=5)))
print(create_enet)
# List the environment to review what functions were created.
print(ls(learners))
# We can simply list the environment to specify the library.
sl = SuperLearner(Y = Y, X = X, SL.library = ls(learners),
                  cvControl = list(V = 2),
                  family = binomial(),
                  env = learners)
print(sl)

####################
# SVM hyperparameters, including a test of character grid elements.

# First remove near-constant X columns to avoid warnings in SVM.
library(caret)
# Remove zero variance (constant) and near-zero-variance columns.
# This can help reduce overfitting and also helps us use a basic glm().
# However, there is a slight risk that we are discarding helpful information.
preproc = caret::preProcess(X, method = c("zv", "nzv"))
X_clean = predict(preproc, X)
rm(preproc)

ncol(X)
ncol(X_clean)

# Test character tuning parameters.
svm = create.Learner("SL.svm", detailed_names = T,
                     tune = list(kernel = c("polynomial", "radial", "sigmoid")))

sl = SuperLearner(Y = Y, X = X_clean,
                  SL.library = c("SL.mean", svm$names),
                  # Using V = 2 causes an error in SVM about an infeasible nu.
                  cvControl = list(V = 3),
                  family = binomial())
print(sl)
