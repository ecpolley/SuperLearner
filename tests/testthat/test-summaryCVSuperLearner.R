
if(all(sapply(c("testthat", "mlbench", "glmnet", "cvAUC", "quadprog"), requireNamespace))){
  
testthat::context("Method: summary.CV.SuperLearner")

############################
# Setup test dataset from mlbench.

data(BreastCancer, package = "mlbench")

data = na.omit(BreastCancer)

set.seed(1)

# Reduce dataset size to speed up testing.
data = data[sample(nrow(data), 140), ]

# Expand out factors into indicators.
X = data.frame(model.matrix(~ . -1, subset(data, select=-c(Id, Class))))

# Restrict to 20 covariates to speed up testing.
X = X[, 1:20]

Y = as.numeric(data$Class == "malignant")
table(Y)

###########################
# Test CV.SL

sl_lib = c("SL.mean", "SL.glmnet")

# Don't specify method, just use the default.
cv_sl = CV.SuperLearner(Y = Y, X = X, family = binomial(),
                        cvControl = list(V = 2), SL.library = sl_lib)
summary(cv_sl)

# Specify method.
cv_sl = CV.SuperLearner(Y = Y, X = X, family = binomial(),
                        cvControl = list(V = 2),
                        SL.library = sl_lib,
                        method = "method.AUC")
summary(cv_sl)

# Specify method using a variable.
method = "method.NNLS2"
cv_sl = CV.SuperLearner(Y = Y, X = X, family = binomial(),
                        cvControl = list(V = 2),
                        SL.library = sl_lib,
                        method = method)
summary(cv_sl)

  }
