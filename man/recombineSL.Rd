\name{recombineSL}
\alias{recombineSL}

\title{Recombine a SuperLearner fit using a new metalearning method}
\description{The \code{recombineSL} function takes an existing SuperLearner fit and a new metalearning method and returns a new SuperLearner fit with updated base learner weights.}

\usage{
recombineSL(object, Y, method = "method.NNloglik", verbose = FALSE)
}

\arguments{
  \item{object}{
Fitted object from \code{SuperLearner}.
}
  \item{Y}{ 
The outcome in the training data set. Must be a numeric vector.
}
  \item{method}{
A list (or a function to create a list) containing details on estimating the coefficients for the super learner and the model to combine the individual algorithms in the library. See \code{?method.template} for details.  Currently, the built in options are either "method.NNLS" (the default), "method.NNLS2", "method.NNloglik", "method.CC_LS", "method.CC_nloglik", or "method.AUC".  NNLS and NNLS2 are non-negative least squares based on the Lawson-Hanson algorithm and the dual method of Goldfarb and Idnani, respectively.  NNLS and NNLS2 will work for both gaussian and binomial outcomes.  NNloglik is a non-negative binomial likelihood maximization using the BFGS quasi-Newton optimization method. NN* methods are normalized so weights sum to one. CC_LS uses Goldfarb and Idnani's quadratic programming algorithm to calculate the best convex combination of weights to minimize the squared error loss. CC_nloglik calculates the convex combination of weights that minimize the negative binomial log likelihood on the logistic scale using the sequential quadratic programming algorithm.  AUC, which only works for binary outcomes, uses the Nelder-Mead method via the optim function to minimize rank loss (equivalent to maximizing AUC).
}
  \item{verbose}{
logical; TRUE for printing progress during the computation (helpful for debugging).
}
}

\details{
  \code{recombineSL} re-fits the super learner prediction algorithm using a new metalearning method.  The weights for each algorithm in \code{SL.library} are re-estimated using the new metalearner, however the base learner fits are not regenerated, so this function saves a lot of computation time as opposed to using the \code{SuperLearner} function with a new \code{method} argument.  The output is identical to the output from the \code{SuperLearner} function.

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
  \item{cvControl}{
The \code{cvControl} list.
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
\author{ Erin LeDell \email{ledell@berkeley.edu} }

\examples{
\dontrun{

# Binary outcome example adapted from SuperLearner examples

set.seed(1)
N <- 200
X <- matrix(rnorm(N*10), N, 10)
X <- as.data.frame(X)
Y <- rbinom(N, 1, plogis(.2*X[, 1] + .1*X[, 2] - .2*X[, 3] + 
  .1*X[, 3]*X[, 4] - .2*abs(X[, 4])))

SL.library <- c("SL.glmnet", "SL.glm", "SL.knn", "SL.gam", "SL.mean")

# least squares loss function
set.seed(1) # for reproducibility
fit_nnls <- SuperLearner(Y = Y, X = X, SL.library = SL.library, 
  verbose = TRUE, method = "method.NNLS", family = binomial())
fit_nnls
#                    Risk       Coef
# SL.glmnet_All 0.2439433 0.01293059
# SL.glm_All    0.2461245 0.08408060
# SL.knn_All    0.2604000 0.09600353
# SL.gam_All    0.2471651 0.40761918
# SL.mean_All   0.2486049 0.39936611


# negative log binomial likelihood loss function
fit_nnloglik <- recombineSL(fit_nnls, Y = Y, method = "method.NNloglik")
fit_nnloglik
#                    Risk      Coef
# SL.glmnet_All 0.6815911 0.1577228
# SL.glm_All    0.6918926 0.0000000
# SL.knn_All          Inf 0.0000000
# SL.gam_All    0.6935383 0.6292881
# SL.mean_All   0.6904050 0.2129891

# If we use the same seed as the original `fit_nnls`, then
# the recombineSL and SuperLearner results will be identical
# however, the recombineSL version will be much faster since
# it doesn't have to re-fit all the base learners.
set.seed(1)
fit_nnloglik2 <- SuperLearner(Y = Y, X = X, SL.library = SL.library,
  verbose = TRUE, method = "method.NNloglik", family = binomial())
fit_nnloglik2
#                    Risk      Coef
# SL.glmnet_All 0.6815911 0.1577228
# SL.glm_All    0.6918926 0.0000000
# SL.knn_All          Inf 0.0000000
# SL.gam_All    0.6935383 0.6292881
# SL.mean_All   0.6904050 0.2129891

}
}
\keyword{models}

