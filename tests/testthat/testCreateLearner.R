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
table(Y)

###########################
# Begin tests.
###########################


########################
# Create a randomForest learner with ntree set to 1000 rather than the default of 500.
create_rf = create.Learner("SL.randomForest", list(ntree = 1000))
create_rf
sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial())
sl
# Clean up global environment.
do.call(rm, as.list(create_rf$names))



########################
# Create a randomForest learner that optimizes over mtry
create_rf = create.Learner("SL.randomForest",
                         tune = list(mtry = round(c(1, sqrt(ncol(X)), ncol(X)))))
create_rf
sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial())
sl
# Clean up global environment.
do.call(rm, as.list(create_rf$names))


########################
# Optimize elastic net over alpha, with a custom environment and detailed names.

learners = new.env()
create_enet = create.Learner("SL.glmnet", env = learners, detailed_names = T,
                                  tune = list(alpha = seq(0, 1, length.out=5)))
create_enet
# List the environment to review what functions were created.
ls(learners)
# Attach the learners environment so that SuperLearner can access the functions.
attach(learners)
# We can simply list the environment to specify the library.
sl = SuperLearner(Y = Y, X = X, SL.library = ls(learners), family = binomial())
sl
detach(learners)
