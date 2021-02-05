# library(testthat)

if(all(sapply(c("testthat", "mlbench", "class"), requireNamespace))){

  testthat::context("Learner: knn")

# Load a test dataset.
data(PimaIndiansDiabetes2, package = "mlbench")

data = PimaIndiansDiabetes2

# Omit observations with missing data.
data = na.omit(data)

Y = as.numeric(data$diabetes)
X = subset(data, select = -diabetes)

set.seed(1)

# Subset to training and test.
train = sample(nrow(X), round(nrow(X) * 0.5))

X_train = X[train, ]
X_test = X[-train, ]

Y_train = Y[train]
Y_test = Y[-train]

# Try just the wrapper itself, not via SuperLearner
knn = SuperLearner::SL.knn(Y_train, X_train, X_test, family = binomial())

# Now try SuperLearner with the wrapper.
sl = SuperLearner(Y_train, X_train, family = binomial(), cvControl = list(V = 2),
                  SL.library = c("SL.mean", "SL.knn"))
sl
# Predict on new data.
pred = predict(sl, X_test, X = X_train, Y = Y_train)
pred

#pred$library.predict[, "SL.knn_All"] == sl$library.predict[, "SL.knn_All"]

  }
