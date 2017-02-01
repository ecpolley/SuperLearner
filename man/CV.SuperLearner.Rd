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
CV.SuperLearner(Y, X, V = NULL, family = gaussian(), SL.library,
  method = "method.NNLS", id = NULL, verbose = FALSE,
  control = list(saveFitLibrary = FALSE), cvControl = list(),
  innerCvControl = list(),              
  obsWeights = NULL, saveAll = TRUE, parallel = "seq", env = parent.frame())
}

\arguments{
  \item{Y}{
The outcome.
}
  \item{X}{
The covariates.
}
  \item{V}{
The number of folds for \code{CV.SuperLearner}. This argument will be depreciated and moved into the \code{cvControl}. If Both \code{V} and \code{cvControl} set the number of cross-validation folds, an error message will appear. The recommendation is to use \code{cvControl}. This is not the number of folds for \code{SuperLearner}. The number of folds for \code{SuperLearner} is controlled with \code{innerCvControl}.
}
  \item{family}{
Currently allows \code{gaussian} or \code{binomial} to describe the error distribution. Link function information will be ignored and should be contained in the method argument below.
}
  \item{SL.library}{
Either a character vector of prediction algorithms or a list containing character vectors. See details below for examples on the structure. A list of functions included in the SuperLearner package can be found with \code{listWrappers()}.
}
  \item{method}{
A list (or a function to create a list) containing details on estimating the coefficients for the super learner and the model to combine the individual algorithms in the library. See \code{?method.template} for details.  Currently, the built in options are either "method.NNLS" (the default), "method.NNLS2", "method.NNloglik", "method.CC_LS", "method.CC_nloglik", or "method.AUC".  NNLS and NNLS2 are non-negative least squares based on the Lawson-Hanson algorithm and the dual method of Goldfarb and Idnani, respectively.  NNLS and NNLS2 will work for both gaussian and binomial outcomes.  NNloglik is a non-negative binomial likelihood maximization using the BFGS quasi-Newton optimization method. NN* methods are normalized so weights sum to one. CC_LS uses Goldfarb and Idnani's quadratic programming algorithm to calculate the best convex combination of weights to minimize the squared error loss. CC_nloglik calculates the convex combination of weights that minimize the negative binomial log likelihood on the logistic scale using the sequential quadratic programming algorithm.  AUC, which only works for binary outcomes, uses the Nelder-Mead method via the optim function to minimize rank loss (equivalent to maximizing AUC).
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
A list of parameters to control the outer cross-validation process. The outer cross-validation is the sample spliting for evaluating the SuperLearner. Parameters include \code{V}, \code{stratifyCV}, \code{shuffle} and \code{validRows}. See \code{\link{SuperLearner.CV.control}} for details.
}
  \item{innerCvControl}{
A list of lists of parameters to control the inner cross-validation process. It should have \code{V} elements in the list, each a valid \code{cvControl} list. If only a single value, then replicated across all folds. The inner cross-validation are the values passed to each of the \code{V} \code{SuperLearner} calls. Parameters include \code{V}, \code{stratifyCV}, \code{shuffle} and \code{validRows}. See \code{\link{SuperLearner.CV.control}} for details.
}
  \item{obsWeights}{
Optional observation weights variable. As with \code{id} above, \code{obsWeights} is passed to the prediction and screening algorithms, but many of the built in wrappers ignore (or can't use) the information. If you are using observation weights, make sure the library you specify uses the information.
}
  \item{saveAll}{
Logical; Should the entire \code{SuperLearner} object be saved for each fold?
}
  \item{parallel}{
Options for parallel computation of the V-fold step. Use "seq" (the default) for sequential computation. \code{parallel = 'multicore'} to use \code{mclapply} for the V-fold step (but note that \code{SuperLearner()} will still be sequential). The default for mclapply is to check the \code{mc.cores} option, and if not set to default to 2 cores. Be sure to set \code{options()$mc.cores} to the desired number of cores if you don't want the default. Or \code{parallel} can be the name of a snow cluster and will use \code{parLapply} for the V-fold step. For both multicore and snow, the inner \code{SuperLearner} calls will be sequential.
  }
  \item{env}{
  Environment containing the learner functions. Defaults to the calling environment.
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


\author{ Eric C Polley \email{polley.eric@mayo.edu} }

\seealso{
\code{\link{SuperLearner}}
}

\examples{
\dontrun{
set.seed(23432)
## training set
n <- 500
p <- 50
X <- matrix(rnorm(n*p), nrow = n, ncol = p)
colnames(X) <- paste("X", 1:p, sep="")
X <- data.frame(X)
Y <- X[, 1] + sqrt(abs(X[, 2] * X[, 3])) + X[, 2] - X[, 3] + rnorm(n)

## build Library and run Super Learner
SL.library <- c("SL.glm", "SL.randomForest", "SL.gam", "SL.polymars", "SL.mean")

test <- CV.SuperLearner(Y = Y, X = X, V = 10, SL.library = SL.library,
  verbose = TRUE, method = "method.NNLS")
test
summary(test)
## Look at the coefficients across folds
coef(test)

# Example with specifying cross-validation options for both 
# CV.SuperLearner (cvControl) and the internal SuperLearners (innerCvControl)
test <- CV.SuperLearner(Y = Y, X = X, SL.library = SL.library,
  cvControl = list(V = 10, shuffle = FALSE),
  innerCvControl = list(list(V = 5)),
  verbose = TRUE, method = "method.NNLS")

## examples with snow
library(parallel)
cl <- makeCluster(2, type = "PSOCK") # can use different types here
clusterSetRNGStream(cl, iseed = 2343)
testSNOW <- CV.SuperLearner(Y = Y, X = X, SL.library = SL.library, method = "method.NNLS",
  parallel = cl)
summary(testSNOW)
stopCluster(cl)
}
}

\keyword{models}
