\name{CV.SuperLearner}
\alias{CV.SuperLearner}
\alias{print.CV.SuperLearner}
\alias{coef.CV.SuperLearner}

\title{
Function to get V-fold cross-validated risk estimate for super learner
}
\description{
Function to get V-fold cross-validated risk estimate for super learner. This function simply splits the data into V folds and then calls SuperLearner. Most of the arguments are passed directly to SuperLearner.
}
\usage{
CV.SuperLearner(Y, X, V = 20, family = gaussian(), SL.library, 
  method = "method.NNLS", id = NULL, verbose = FALSE, 
  control = list(saveFitLibrary = FALSE), cvControl = list(), 
  obsWeights = NULL, saveAll = TRUE, parallel = "seq")
}

\arguments{
  \item{Y}{
The outcome.
}
  \item{X}{
The covariates.
}
  \item{V}{
The number of folds for \code{CV.SuperLearner}. This is not the number of folds for \code{SuperLearner}. The number of folds for \code{SuperLearner} is controlled with \code{cvControl}.
}
  \item{family}{
Currently allows \code{gaussian} or \code{binomial} to describe the error distribution. Link function information will be ignored and should be contained in the method argument below.
}
  \item{SL.library}{
Either a character vector of prediction algorithms or a list containing character vectors. See details below for examples on the structure. A list of functions included in the SuperLearner package can be found with \code{listWrappers()}.
}
  \item{method}{
A list (or a function to create a list) containing details on estimating the coefficients for the super learner and the model to combine the individual algorithms in the library. See \code{?method.template} for details.  Currently, the built in options are either "method.NNLS" (the default), "method.NNLS2", or "method.NNloglik".  NNLS and NNLS2 are non-negative least squares based on the Lawson-Hanson algorithm and the dual method of Goldfarb and Idnani, respectively.  NNLS and NNLS2 will work for both gaussian and binomial outcomes.  NNloglik is a non-negative binomial likelihood maximization using the BFGS quasi-Newton optimization method.
}
  \item{id}{
Optional cluster identification variable. For the cross-validation splits, \code{id} forces observations in the same cluster to be in the same validation fold. \code{id} is passed to the prediction and screening algorithms in SL.library, but be sure to check the individual wrappers as many of them ignore the information.
}
  \item{verbose}{
Logical; TRUE for printing progress during the computation (helpful for debugging).
}
  \item{control}{
A list of parameters to control the estimation process. Parameters include \code{saveFitLibrary} and \code{trimLogit}. See \code{\link{SuperLearner.control}} for details.
}
  \item{cvControl}{
A list of parameters to control the cross-validation process. Parameters include \code{V}, \code{stratifyCV}, \code{shuffle} and \code{validRows}. See \code{\link{SuperLearner.CV.control}} for details.
}
  \item{obsWeights}{
Optional observation weights variable. As with \code{id} above, \code{obsWeights} is passed to the prediction and screening algorithms, but many of the built in wrappers ignore (or can't use) the information. If you are using observation weights, make sure the library you specify uses the information.
}
  \item{saveAll}{
Logical; Should the entire \code{SuperLearner} object be saved for each fold?
}
  \item{parallel}{
Options for parallel computation of the V-fold step. Use "seq" (the default) for sequential computation. \code{parallel = 'multicore'} to use \code{mclapply} for the V-fold step (but note that \code{SuperLearner()} will still be sequential). Or \code{parallel} can be the name of a snow cluster and will use \code{parLapply} for the V-fold step. For both multicore and snow, the inner \code{SuperLearner} calls will be sequential.
  }
}

\details{
The \code{SuperLearner} function builds a estimator, but does not contain an estimate on the performance of the estimator. Various methods exist for estimator performance evaluation. If you are familiar with the super learner algorithm, it should be no surprise we recommend using cross-validation to evaluate the honest performance of the super learner estimator. The function \code{CV.SuperLearner} computes the usual V-fold cross-validated risk estimate for the super learner (and all algorithms in \code{SL.library} for comparison).
}
\value{
An object of class \code{CV.SuperLearner} (a list) with components:

  \item{call}{
The matched call.
}
\item{AllSL}{
If \code{saveAll = TRUE}, a list with output from each call to \code{SuperLearner}, otherwise NULL.
}
\item{SL.predict}{
The predicted values from the super learner when each particular row was part of the validation fold.
}
\item{discreteSL.predict}{
The traditional cross-validated selector. Picks the algorithm with the smallest cross-validated risk (in super learner terms, gives that algorithm coefficient 1 and all others 0).
}
\item{whichDiscreteSL}{
A list of length \code{V}. The elements in the list are the algorithm that had the smallest cross-validated risk estimate for that fold.
}
\item{library.predict}{
A matrix with the predicted values from each algorithm in \code{SL.library}. The columns are the algorithms in \code{SL.library} and the rows represent the predicted values when that particular row was in the validation fold (i.e. not used to fit that estimator).
}
\item{coef}{
A matrix with the coefficients for the super learner on each fold. The columns are the algorithms in \code{SL.library} the rows are the folds.
}
\item{folds}{
A list containing the row numbers for each validation fold.
}
\item{V}{
Number of folds for \code{CV.SuperLearner}.
}
  \item{libraryNames}{
A character vector with the names of the algorithms in the library. The format is 'predictionAlgorithm_screeningAlgorithm' with '_All' used to denote the prediction algorithm run on all variables in X.
}
  \item{SL.library}{
Returns \code{SL.library} in the same format as the argument with the same name above.
}
\item{method}{
A list with the method functions.
}
\item{Y}{
The outcome
}
}


\author{ Eric C Polley \email{eric.polley@nih.gov} }

\seealso{
\code{\link{SuperLearner}}
}

\examples{
set.seed(23432)
## training set
n <- 500
p <- 50
X <- matrix(rnorm(n*p), nrow = n, ncol = p)
colnames(X) <- paste("X", 1:p, sep="")
X <- data.frame(X)
Y <- X[, 1] + sqrt(abs(X[, 2] * X[, 3])) + X[, 2] - X[, 3] + rnorm(n)
  
# build Library and run Super Learner
SL.library <- c("SL.glm", "SL.randomForest", "SL.gam", "SL.polymars", "SL.mean")
\dontrun{
test <- CV.SuperLearner(Y = Y, X = X, V = 10, SL.library = SL.library,
  verbose = TRUE, method = "method.NNLS")
test
summary(test)
# Look at the coefficients across folds
coef(test)
}
}

\keyword{models}
