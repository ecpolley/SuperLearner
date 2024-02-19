\name{SuperLearner}
\alias{SuperLearner}
\alias{mcSuperLearner}
\alias{snowSuperLearner}
\alias{print.SuperLearner}
\alias{coef.SuperLearner}

\title{Super Learner Prediction Function}
\description{A Prediction Function for the Super Learner.  The \code{SuperLearner} function takes a training set pair (X,Y) and returns the predicted values based on a validation set.}

\usage{
SuperLearner(Y, X, newX = NULL, family = gaussian(), SL.library,
  method = "method.NNLS", id = NULL, verbose = FALSE,
  control = list(), cvControl = list(), obsWeights = NULL, env = parent.frame())
}

\arguments{
  \item{Y}{
The outcome in the training data set. Must be a numeric vector.
}
  \item{X}{
The predictor variables in the training data set, usually a data.frame.
}
  \item{newX}{
The predictor variables in the validation data set. The structure should match X. If missing, uses X for newX.
}
  \item{SL.library}{
Either a character vector of prediction algorithms or a list containing character vectors. See details below for examples on the structure. A list of functions included in the SuperLearner package can be found with \code{listWrappers()}.}
  \item{verbose}{
logical; TRUE for printing progress during the computation (helpful for debugging).
}
  \item{family}{
Currently allows \code{gaussian} or \code{binomial} to describe the error distribution. Link function information will be ignored and should be contained in the method argument below.
}
  \item{method}{
A list (or a function to create a list) containing details on estimating the coefficients for the super learner and the model to combine the individual algorithms in the library. See \code{?method.template} for details.  Currently, the built in options are either "method.NNLS" (the default), "method.NNLS2", "method.NNloglik", "method.CC_LS", "method.CC_nloglik", or "method.AUC".  NNLS and NNLS2 are non-negative least squares based on the Lawson-Hanson algorithm and the dual method of Goldfarb and Idnani, respectively.  NNLS and NNLS2 will work for both gaussian and binomial outcomes.  NNloglik is a non-negative binomial likelihood maximization using the BFGS quasi-Newton optimization method. NN* methods are normalized so weights sum to one. CC_LS uses Goldfarb and Idnani's quadratic programming algorithm to calculate the best convex combination of weights to minimize the squared error loss. CC_nloglik calculates the convex combination of weights that minimize the negative binomial log likelihood on the logistic scale using the sequential quadratic programming algorithm.  AUC, which only works for binary outcomes, uses the Nelder-Mead method via the optim function to minimize rank loss (equivalent to maximizing AUC).
}
  \item{id}{
Optional cluster identification variable. For the cross-validation splits, \code{id} forces observations in the same cluster to be in the same validation fold. \code{id} is passed to the prediction and screening algorithms in SL.library, but be sure to check the individual wrappers as many of them ignore the information.
}
  \item{obsWeights}{
Optional observation weights variable. As with \code{id} above, \code{obsWeights} is passed to the prediction and screening algorithms, but many of the built in wrappers ignore (or can't use) the information. If you are using observation weights, make sure the library you specify uses the information.
}
  \item{control}{
A list of parameters to control the estimation process. Parameters include \code{saveFitLibrary} and \code{trimLogit}. See \code{\link{SuperLearner.control}} for details.
}
  \item{cvControl}{
A list of parameters to control the cross-validation process. Parameters include \code{V}, \code{stratifyCV}, \code{shuffle} and \code{validRows}. See \code{\link{SuperLearner.CV.control}} for details.
}

  \item{env}{
  Environment containing the learner functions. Defaults to the calling environment.
  }
}

\details{
  \code{SuperLearner} fits the super learner prediction algorithm.  The weights for each algorithm in \code{SL.library} is estimated, along with the fit of each algorithm.

The prescreen algorithms.  These algorithms first rank the variables in \code{X} based on either a univariate regression p-value of the \code{randomForest} variable importance.  A subset of the variables in \code{X} is selected based on a pre-defined cut-off.  With this subset of the X variables, the algorithms in \code{SL.library} are then fit.

The SuperLearner package contains a few prediction and screening algorithm wrappers. The full list of wrappers can be viewed with \code{listWrappers()}. The design of the SuperLearner package is such that the user can easily add their own wrappers. We also maintain a website with additional examples of wrapper functions at \url{https://github.com/ecpolley/SuperLearnerExtra}.
}
\value{
  \item{call}{
The matched call.
}
  \item{libraryNames}{
A character vector with the names of the algorithms in the library. The format is 'predictionAlgorithm_screeningAlgorithm' with '_All' used to denote the prediction algorithm run on all variables in X.
}
  \item{SL.library}{
Returns \code{SL.library} in the same format as the argument with the same name above.
}
  \item{SL.predict}{
The predicted values from the super learner for the rows in \code{newX}.
}
  \item{coef}{
Coefficients for the super learner.
}
  \item{library.predict}{
A matrix with the predicted values from each algorithm in \code{SL.library} for the rows in \code{newX}.
}
  \item{Z}{
The Z matrix (the cross-validated predicted values for each algorithm in \code{SL.library}).
}
  \item{cvRisk}{
A numeric vector with the V-fold cross-validated risk estimate for each algorithm in \code{SL.library}. Note that this does not contain the CV risk estimate for the SuperLearner, only the individual algorithms in the library.
}
  \item{family}{
Returns the \code{family} value from above
}
  \item{fitLibrary}{
A list with the fitted objects for each algorithm in \code{SL.library} on the full training data set.
}
  \item{cvFitLibrary}{
A list with fitted objects for each algorithm in \code{SL.library} on each of 
\code{V} different training data sets. 
}
  \item{varNames}{
A character vector with the names of the variables in \code{X}.
}
  \item{validRows}{
A list containing the row numbers for the V-fold cross-validation step.
}
  \item{method}{
A list with the method functions.
}
  \item{whichScreen}{
A logical matrix indicating which variables passed each screening algorithm.
}
  \item{control}{
The \code{control} list.
}
  \item{cvControl}{
The \code{cvControl} list.
}
  \item{errorsInCVLibrary}{
A logical vector indicating if any algorithms experienced an error within the CV step.
}
  \item{errorsInLibrary}{
A logical vector indicating if any algorithms experienced an error on the full data.
}
  \item{env}{
  Environment passed into function which will be searched to find the learner functions. Defaults to the calling environment.
  }
  \item{times}{
  A list that contains the execution time of the SuperLearner, plus separate times for model fitting and prediction.
  }
}

\references{
	van der Laan, M. J., Polley, E. C. and Hubbard, A. E. (2008) Super Learner, \emph{Statistical Applications of Genetics and Molecular Biology}, \bold{6}, article 25.
}
\author{ Eric C Polley \email{epolley@uchicago.edu} }

\examples{
\dontrun{
## simulate data
set.seed(23432)
## training set
n <- 500
p <- 50
X <- matrix(rnorm(n*p), nrow = n, ncol = p)
colnames(X) <- paste("X", 1:p, sep="")
X <- data.frame(X)
Y <- X[, 1] + sqrt(abs(X[, 2] * X[, 3])) + X[, 2] - X[, 3] + rnorm(n)

## test set
m <- 1000
newX <- matrix(rnorm(m*p), nrow = m, ncol = p)
colnames(newX) <- paste("X", 1:p, sep="")
newX <- data.frame(newX)
newY <- newX[, 1] + sqrt(abs(newX[, 2] * newX[, 3])) + newX[, 2] -
  newX[, 3] + rnorm(m)

# generate Library and run Super Learner
SL.library <- c("SL.glm", "SL.randomForest", "SL.gam",
  "SL.polymars", "SL.mean")
test <- SuperLearner(Y = Y, X = X, newX = newX, SL.library = SL.library,
  verbose = TRUE, method = "method.NNLS")
test

# library with screening
SL.library <- list(c("SL.glmnet", "All"), c("SL.glm", "screen.randomForest",
  "All", "screen.SIS"), "SL.randomForest", c("SL.polymars", "All"), "SL.mean")
test <- SuperLearner(Y = Y, X = X, newX = newX, SL.library = SL.library,
  verbose = TRUE, method = "method.NNLS")
test

# binary outcome
set.seed(1)
N <- 200
X <- matrix(rnorm(N*10), N, 10)
X <- as.data.frame(X)
Y <- rbinom(N, 1, plogis(.2*X[, 1] + .1*X[, 2] - .2*X[, 3] +
  .1*X[, 3]*X[, 4] - .2*abs(X[, 4])))

SL.library <- c("SL.glmnet", "SL.glm", "SL.knn", "SL.gam", "SL.mean")

# least squares loss function
test.NNLS <- SuperLearner(Y = Y, X = X, SL.library = SL.library,
  verbose = TRUE, method = "method.NNLS", family = binomial())
test.NNLS

# negative log binomial likelihood loss function
test.NNloglik <- SuperLearner(Y = Y, X = X, SL.library = SL.library,
  verbose = TRUE, method = "method.NNloglik", family = binomial())
test.NNloglik

# 1 - AUC loss function
test.AUC <- SuperLearner(Y = Y, X = X, SL.library = SL.library,
  verbose = TRUE, method = "method.AUC", family = binomial())
test.AUC

# 2
# adapted from library(SIS)
set.seed(1)
# training
b <- c(2, 2, 2, -3*sqrt(2))
n <- 150
p <- 200
truerho <- 0.5
corrmat <- diag(rep(1-truerho, p)) + matrix(truerho, p, p)
corrmat[, 4] = sqrt(truerho)
corrmat[4, ] = sqrt(truerho)
corrmat[4, 4] = 1
cholmat <- chol(corrmat)
x <- matrix(rnorm(n*p, mean=0, sd=1), n, p)
x <- x %*% cholmat
feta <- x[, 1:4] %*% b
fprob <- exp(feta) / (1 + exp(feta))
y <- rbinom(n, 1, fprob)

# test
m <- 10000
newx <- matrix(rnorm(m*p, mean=0, sd=1), m, p)
newx <- newx %*% cholmat
newfeta <- newx[, 1:4] %*% b
newfprob <- exp(newfeta) / (1 + exp(newfeta))
newy <- rbinom(m, 1, newfprob)

DATA2 <- data.frame(Y = y, X = x)
newDATA2 <- data.frame(Y = newy, X=newx)

create.SL.knn <- function(k = c(20, 30)) {
  for(mm in seq(length(k))){
    eval(parse(text = paste('SL.knn.', k[mm], '<- function(..., k = ', k[mm],
      ') SL.knn(..., k = k)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.knn(c(20, 30, 40, 50, 60, 70))

# library with screening
SL.library <- list(c("SL.glmnet", "All"), c("SL.glm", "screen.randomForest"),
  "SL.randomForest", "SL.knn", "SL.knn.20", "SL.knn.30", "SL.knn.40",
  "SL.knn.50", "SL.knn.60", "SL.knn.70",
  c("SL.polymars", "screen.randomForest"))
test <- SuperLearner(Y = DATA2$Y, X = DATA2[, -1], newX = newDATA2[, -1],
  SL.library = SL.library, verbose = TRUE, family = binomial())
test

## examples with multicore
set.seed(23432, "L'Ecuyer-CMRG")  # use L'Ecuyer for multicore seeds. see ?set.seed for details
## training set
n <- 500
p <- 50
X <- matrix(rnorm(n*p), nrow = n, ncol = p)
colnames(X) <- paste("X", 1:p, sep="")
X <- data.frame(X)
Y <- X[, 1] + sqrt(abs(X[, 2] * X[, 3])) + X[, 2] - X[, 3] + rnorm(n)

## test set
m <- 1000
newX <- matrix(rnorm(m*p), nrow = m, ncol = p)
colnames(newX) <- paste("X", 1:p, sep="")
newX <- data.frame(newX)
newY <- newX[, 1] + sqrt(abs(newX[, 2] * newX[, 3])) + newX[, 2] - newX[, 3] + rnorm(m)

# generate Library and run Super Learner
SL.library <- c("SL.glm", "SL.randomForest", "SL.gam",
  "SL.polymars", "SL.mean")

testMC <- mcSuperLearner(Y = Y, X = X, newX = newX, SL.library = SL.library,
  method = "method.NNLS")
testMC

## examples with snow
library(parallel)
cl <- makeCluster(2, type = "PSOCK") # can use different types here
clusterSetRNGStream(cl, iseed = 2343)
# make SL functions available on the clusters, use assignment to avoid printing
foo <- clusterEvalQ(cl, library(SuperLearner))  
testSNOW <- snowSuperLearner(cluster = cl, Y = Y, X = X, newX = newX,
  SL.library = SL.library, method = "method.NNLS")
testSNOW
stopCluster(cl)

## snow example with user-generated wrappers
# If you write your own wrappers and are using snowSuperLearner()
# These new wrappers need to be added to the SuperLearner namespace and exported to the clusters
# Using a simple example here, but can define any new SuperLearner wrapper
my.SL.wrapper <- function(...) SL.glm(...)
# assign function into SuperLearner namespace
environment(my.SL.wrapper) <-asNamespace("SuperLearner")

cl <- makeCluster(2, type = "PSOCK") # can use different types here
clusterSetRNGStream(cl, iseed = 2343)
# make SL functions available on the clusters, use assignment to avoid printing	
foo <- clusterEvalQ(cl, library(SuperLearner))  
clusterExport(cl, c("my.SL.wrapper"))  # copy the function to all clusters
testSNOW <- snowSuperLearner(cluster = cl, Y = Y, X = X, newX = newX,
  SL.library = c("SL.glm", "SL.mean", "my.SL.wrapper"), method = "method.NNLS")
testSNOW
stopCluster(cl)

## timing
replicate(5, system.time(SuperLearner(Y = Y, X = X, newX = newX,
  SL.library = SL.library, method = "method.NNLS")))

replicate(5, system.time(mcSuperLearner(Y = Y, X = X, newX = newX,
  SL.library = SL.library, method = "method.NNLS")))

cl <- makeCluster(2, type = 'PSOCK')
# make SL functions available on the clusters, use assignment to avoid printing	
foo <- clusterEvalQ(cl, library(SuperLearner))  
replicate(5, system.time(snowSuperLearner(cl, Y = Y, X = X, newX = newX,
  SL.library = SL.library, method = "method.NNLS")))
stopCluster(cl)

}
}
\keyword{models}

