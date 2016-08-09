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
X = data.frame(model.matrix(~ . -1, subset(data, select=-c(Class))))

Y = as.numeric(data$Class == "malignant")
print(table(Y))

###########################
# Begin tests.
###########################


########################
# Create a randomForest learner with ntree set to 1000 rather than the default of 500.
create_rf = create.Learner("SL.randomForest", list(ntree = 1000))
print(create_rf)
print(ls())
sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial())
print(sl)

# Clean up global environment.
rm(list=create_rf$names)



########################
# Create a randomForest learner that optimizes over mtry
create_rf = create.Learner("SL.randomForest",
                         tune = list(mtry = round(c(1, sqrt(ncol(X)), ncol(X)))))
print(create_rf)
sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial())
print(sl)

# Clean up global environment.
rm(list=create_rf$names)


########################
# Optimize elastic net over alpha, with a custom environment and detailed names.

learners = new.env()
create_enet = create.Learner("SL.glmnet", env = learners, detailed_names = T,
                                  tune = list(alpha = seq(0, 1, length.out=5)))
print(create_enet)
# List the environment to review what functions were created.
print(ls(learners))
# We can simply list the environment to specify the library.
sl = SuperLearner(Y = Y, X = X, SL.library = ls(learners), family = binomial(), env = learners)
sl
