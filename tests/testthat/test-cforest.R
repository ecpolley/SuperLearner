library(testthat)
library(SuperLearner)
library(party)

context("Learner: cforest")

############################
# Setup test dataset from mlbench.

data(BreastCancer, package="mlbench")

data = na.omit(BreastCancer)

set.seed(1)

# Reduce to a dataset of 100 observations to speed up testing.
data = data[sample(nrow(data), 100), ]

# Expand out factors into indicators.
X_bc = data.frame(model.matrix(~ . -1, subset(data, select=-c(Id, Class))))

# Limit to 20 variables to speed up testing.
X_bc = X_bc[, 1:20]

Y_bc = as.numeric(data$Class == "malignant")
table(Y_bc)

##########################
# Run a raw cforest without SL.
ntree = 20
mtry = max(floor(ncol(X_bc) / 3), 1)
mtry
controls = controls = party::cforest_unbiased(ntree = ntree, mtry = mtry)
cf = party::cforest(Y ~ ., data = data.frame(Y=Y_bc, X_bc), controls = controls)

# Estimate OOB predictions.
pred = predict(cf, OOB=T)
summary(pred)

# Calculate AUC.
rocr_pred = ROCR::prediction(pred, Y_bc)
perf = ROCR::performance(rocr_pred, "tpr", "fpr")
auc = ROCR::performance(rocr_pred, measure = "auc", x.measure = "cutoff")@y.values[[1]]
# Not too shabby!
auc

###########################
# Test basic SL ensemble with cforest - classification

sl_lib = c("SL.cforest", "SL.mean")

sl = SuperLearner(Y = Y_bc, X = X_bc, SL.library = sl_lib,
                  cvControl = list(V = 2),
                  family = binomial())
sl

###########################
# Test regression

set.seed(1)
N = 200
X = as.data.frame(matrix(rnorm(N*10), N, 10))

Y_reg <- .2*X[, 1] + .1*X[, 2] - .2*X[, 3] + .1*X[, 3]*X[, 4] - .2*abs(X[, 4]) + rnorm(N)
summary(Y_reg)

sl = SuperLearner(Y = Y_reg, X = X, SL.library = sl_lib,
                  cvControl = list(V = 2),
                  family = gaussian())
sl
