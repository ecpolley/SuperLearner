########################
# Simulate data
set.seed(23432)
# Create small training set for quick tests.
n <- 80
p <- 6
X <- data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
colnames(X) <- paste0("X", 1:p)
gen_y = function(x) x[, 1] + sqrt(abs(x[, 2] * x[, 3])) + x[, 2] - x[, 3] + rnorm(nrow(x))
Y <- gen_y(X)
summary(Y)


########################
# Run the SuperLearner
models = c("SL.randomForest", "SL.glmnet", "SL.glm", "SL.mean")
cv_control = SuperLearner.CV.control(V = 2L)
sl = SuperLearner(Y, X, SL.library = models, cvControl = cv_control)
sl
# Check execution times
sl$times
