library(testthat)

context("Learner: kernelKnn")

# Load a test dataset.
data(PimaIndiansDiabetes2, package = "mlbench")

data = PimaIndiansDiabetes2

# Omit observations with missing data.
data = na.omit(data)

Y = as.numeric(data$diabetes)
X = subset(data, select = -diabetes)

set.seed(1)

# Try just the wrapper itself, not via SuperLearner
knn = SuperLearner::SL.kernelKnn(Y, X, X, family = binomial())

# Now try SuperLearner with the wrapper.
sl = SuperLearner(Y, X, family = binomial(),
                  SL.library = c("SL.mean", "SL.kernelKnn"))
sl
