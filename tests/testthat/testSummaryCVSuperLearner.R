library(testthat)

context("Method: summary.CV.SuperLearner")

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
# Test CV.SL

sl_lib = c("SL.mean", "SL.glmnet")

# Don't specify method, just use the default.
cv_sl = CV.SuperLearner(Y = Y, X = X, family = binomial(), V = 4, SL.library = sl_lib)
summary(cv_sl)

# Specify method.
cv_sl = CV.SuperLearner(Y = Y, X = X, family = binomial(), V = 4, SL.library = sl_lib,
                        method = "method.AUC")
summary(cv_sl)

# Specify method using a variable.
method = "method.NNLS2"
cv_sl = CV.SuperLearner(Y = Y, X = X, family = binomial(), V = 4, SL.library = sl_lib,
                        method = method)
summary(cv_sl)
