library(testthat)

context("Learner: kernelKnn")

# Load a test dataset.
data(PimaIndiansDiabetes2, package = "mlbench")

data = PimaIndiansDiabetes2

# Omit observations with missing data.
data = na.omit(data)

Y_bin = as.numeric(data$diabetes)
X = subset(data, select = -diabetes)

set.seed(1)

# Try just the wrapper itself, not via SuperLearner
knn = SuperLearner::SL.kernelKnn(Y_bin, X, X, family = binomial())

# Now try SuperLearner with the wrapper.
sl = SuperLearner(Y_bin, X, family = binomial(), cvControl = list(V = 2),
                  SL.library = c("SL.mean", "SL.kernelKnn"))
sl

######
# Test gaussian() outcome.

Y_gaus = data$glucose
X = subset(data, select = -glucose)
X$diabetes = as.numeric(X$diabetes == "pos")

set.seed(1)

# Try just the wrapper itself, not via SuperLearner
knn = SuperLearner::SL.kernelKnn(Y_gaus, X, X, family = gaussian())

# Now try SuperLearner with the wrapper.
sl = SuperLearner(Y_gaus, X, family = gaussian(), cvControl = list(V = 2),
                  SL.library = c("SL.mean", "SL.kernelKnn"))
sl

