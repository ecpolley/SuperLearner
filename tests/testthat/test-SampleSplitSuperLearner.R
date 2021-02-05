library(SuperLearner)
if(all(sapply(c("testthat", "randomForest", "glmnet"), requireNamespace))) {

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

########################
# Run the SampleSplitSuperLearner
SL.library = c("SL.randomForest", "SL.glmnet", "SL.glm", "SL.mean")

# default split: 80% in training, 20% in validation
defaultSplit = formals(SampleSplitSuperLearner)$split
sssl1 = SampleSplitSuperLearner(Y = Y, X = X, SL.library = SL.library)
testthat::expect_equal(length(sssl1$validRows), (1 - defaultSplit) * n)
testthat::expect_false(any(sssl1$errorsInLibrary))
testthat::expect_false(any(sssl1$errorsInCVLibrary))

# split by specifying row numbers in validation set
validRows = seq(from = round(n/5), to = n, by = round(n/10))
sssl2 = SampleSplitSuperLearner(Y = Y, X = X, SL.library = SL.library,
								split = validRows)
testthat::expect_equal(sssl2$validRows, validRows)
testthat::expect_false(any(sssl2$errorsInLibrary))
testthat::expect_false(any(sssl2$errorsInCVLibrary))

# split with only one observation in validation set
validRows = 1
sssl3 = SampleSplitSuperLearner(Y = Y, X = X, SL.library = SL.library,
								split = validRows)
testthat::expect_equal(sssl3$validRows, validRows)
testthat::expect_false(any(sssl3$errorsInLibrary))
testthat::expect_false(any(sssl3$errorsInCVLibrary))
	
}
