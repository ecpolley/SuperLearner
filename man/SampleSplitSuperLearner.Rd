\name{SampleSplitSuperLearner}
\alias{SampleSplitSuperLearner}

\title{Super Learner Prediction Function}
\description{A Prediction Function for the Super Learner.  The \code{SuperLearner} function takes a training set pair (X,Y) and returns the predicted values based on a validation set. SampleSplitSuperLearner uses sample split validation whereas SuperLearner uses V-fold cross-validation.}

\usage{
SampleSplitSuperLearner(Y, X, newX = NULL, family = gaussian(), SL.library, 
  method = "method.NNLS", id = NULL, verbose = FALSE, 
  control = list(), split = 0.8, obsWeights = NULL)
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
A list (or a function to create a list) containing details on estimating the coefficients for the super learner and the model to combine the individual algorithms in the library. See \code{?method.template} for details.  Currently, the built in options are either "method.NNLS" (the default), "method.NNLS2", "method.NNloglik", "method.CC_LS", or "method.CC_nloglik".  NNLS and NNLS2 are non-negative least squares based on the Lawson-Hanson algorithm and the dual method of Goldfarb and Idnani, respectively.  NNLS and NNLS2 will work for both gaussian and binomial outcomes.  NNloglik is a non-negative binomial likelihood maximization using the BFGS quasi-Newton optimization method. NN* methods are normalized so weights sum to one. CC_LS uses Goldfarb and Idnani's quadratic programming algorithm to calculate the best convex combination of weights to minimize the squared error loss. CC_nloglik calculates the convex combination of weights that minimize the negative binomial log likelihood on the logistic scale using the sequential quadratic programming algorithm. 
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
  \item{split}{
Either a single value between 0 and 1 indicating the fraction of the samples for the training split. A value of 0.8 will randomly assign 80 percent of the samples to the training split and the other 20 percent to the validation split. Alternatively, split can be a numeric vector with the row numbers of \code{X} corresponding to the validation split. All other rows not in the vector will be considered in the training split.
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
  \item{split}{
The \code{split} value.
}
  \item{errorsInCVLibrary}{
A logical vector indicating if any algorithms experienced an error within the CV step.
}
  \item{errorsInLibrary}{
A logical vector indicating if any algorithms experienced an error on the full data.
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
test <- SampleSplitSuperLearner(Y = Y, X = X, newX = newX, SL.library = SL.library,
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
test.NNLS <- SampleSplitSuperLearner(Y = Y, X = X, SL.library = SL.library, 
  verbose = TRUE, method = "method.NNLS", family = binomial())
test.NNLS
}
}
\keyword{models}

