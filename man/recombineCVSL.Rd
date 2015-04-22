\name{recombineCVSL}
\alias{recombineCVSL}

\title{
Recombine a CV.SuperLearner fit using a new metalearning method
}
\description{
Function to re-compute the V-fold cross-validated risk estimate for super learner using a new metalearning method.  This function takes as input an existing CV.SuperLearner fit and applies the \code{recombineSL} fit to each of the V Super Learner fits.
}
\usage{
recombineCVSL(object, method = "method.NNloglik", verbose = FALSE, 
  saveAll = TRUE, parallel = "seq")
}

\arguments{
  \item{object}{
Fitted object from \code{CV.SuperLearner}. 
}
  \item{method}{
A list (or a function to create a list) containing details on estimating the coefficients for the super learner and the model to combine the individual algorithms in the library. See \code{?method.template} for details.  Currently, the built in options are either "method.NNLS" (the default), "method.NNLS2", "method.NNloglik", "method.CC_LS", "method.CC_nloglik", or "method.AUC".  NNLS and NNLS2 are non-negative least squares based on the Lawson-Hanson algorithm and the dual method of Goldfarb and Idnani, respectively.  NNLS and NNLS2 will work for both gaussian and binomial outcomes.  NNloglik is a non-negative binomial likelihood maximization using the BFGS quasi-Newton optimization method. NN* methods are normalized so weights sum to one. CC_LS uses Goldfarb and Idnani's quadratic programming algorithm to calculate the best convex combination of weights to minimize the squared error loss. CC_nloglik calculates the convex combination of weights that minimize the negative binomial log likelihood on the logistic scale using the sequential quadratic programming algorithm.  AUC, which only works for binary outcomes, uses the Nelder-Mead method via the optim function to minimize rank loss (equivalent to maximizing AUC).
}
  \item{verbose}{
logical; TRUE for printing progress during the computation (helpful for debugging).
}

  \item{saveAll}{
Logical; Should the entire \code{SuperLearner} object be saved for each fold?
}
  \item{parallel}{
Options for parallel computation of the V-fold step. Use "seq" (the default) for sequential computation. \code{parallel = 'multicore'} to use \code{mclapply} for the V-fold step (but note that \code{SuperLearner()} will still be sequential). Or \code{parallel} can be the name of a snow cluster and will use \code{parLapply} for the V-fold step. For both multicore and snow, the inner \code{SuperLearner} calls will be sequential.
  }
}

\details{
 The function \code{recombineCVSL} computes the usual V-fold cross-validated risk estimate for the super learner (and all algorithms in \code{SL.library} for comparison), using a newly specified metalearning method. The weights for each algorithm in \code{SL.library} are re-estimated using the new metalearner, however the base learner fits are not regenerated, so this function saves a lot of computation time as opposed to using the \code{CV.SuperLearner} function with a new \code{method} argument.  The output is identical to the output from the \code{CV.SuperLearner} function.
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


\author{ Erin LeDell \email{ledell@berkeley.edu} }

\seealso{
\code{\link{recombineSL}}
}

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
cvfit_nnls <- CV.SuperLearner(Y = Y, X = X, V = 10, SL.library = SL.library, 
  verbose = TRUE, method = "method.NNLS", family = binomial())
cvfit_nnls$coef
#    SL.glmnet_All SL.glm_All  SL.knn_All SL.gam_All SL.mean_All
# 1      0.0000000 0.00000000 0.000000000  0.4143862   0.5856138
# 2      0.0000000 0.00000000 0.304802397  0.3047478   0.3904498
# 3      0.0000000 0.00000000 0.002897533  0.5544075   0.4426950
# 4      0.0000000 0.20322642 0.000000000  0.1121891   0.6845845
# 5      0.1743973 0.00000000 0.032471026  0.3580624   0.4350693
# 6      0.0000000 0.00000000 0.099881535  0.3662309   0.5338876
# 7      0.0000000 0.00000000 0.234876082  0.2942472   0.4708767
# 8      0.0000000 0.06424676 0.113988158  0.5600208   0.2617443
# 9      0.0000000 0.00000000 0.338030342  0.2762604   0.3857093
# 10     0.3022442 0.00000000 0.294226204  0.1394534   0.2640762


# negative log binomial likelihood loss function
cvfit_nnloglik <- recombineCVSL(cvfit_nnls, method = "method.NNloglik")
cvfit_nnloglik$coef
#    SL.glmnet_All SL.glm_All SL.knn_All SL.gam_All SL.mean_All
# 1      0.0000000  0.0000000 0.00000000  0.5974799  0.40252010
# 2      0.0000000  0.0000000 0.31177345  0.6882266  0.00000000
# 3      0.0000000  0.0000000 0.01377469  0.8544238  0.13180152
# 4      0.0000000  0.1644188 0.00000000  0.2387919  0.59678930
# 5      0.2142254  0.0000000 0.00000000  0.3729426  0.41283197
# 6      0.0000000  0.0000000 0.00000000  0.5847150  0.41528502
# 7      0.0000000  0.0000000 0.47538172  0.5080311  0.01658722
# 8      0.0000000  0.0000000 0.00000000  1.0000000  0.00000000
# 9      0.0000000  0.0000000 0.45384961  0.2923480  0.25380243
# 10     0.3977816  0.0000000 0.27927906  0.1606384  0.16230097

# If we use the same seed as the original `cvfit_nnls`, then
# the recombineCVSL and CV.SuperLearner results will be identical
# however, the recombineCVSL version will be much faster since
# it doesn't have to re-fit all the base learners, V times each.
set.seed(1)
cvfit_nnloglik2 <- CV.SuperLearner(Y = Y, X = X, V = 10, SL.library = SL.library,
  verbose = TRUE, method = "method.NNloglik", family = binomial())
cvfit_nnloglik2$coef
#    SL.glmnet_All SL.glm_All SL.knn_All SL.gam_All SL.mean_All
# 1      0.0000000  0.0000000 0.00000000  0.5974799  0.40252010
# 2      0.0000000  0.0000000 0.31177345  0.6882266  0.00000000
# 3      0.0000000  0.0000000 0.01377469  0.8544238  0.13180152
# 4      0.0000000  0.1644188 0.00000000  0.2387919  0.59678930
# 5      0.2142254  0.0000000 0.00000000  0.3729426  0.41283197
# 6      0.0000000  0.0000000 0.00000000  0.5847150  0.41528502
# 7      0.0000000  0.0000000 0.47538172  0.5080311  0.01658722
# 8      0.0000000  0.0000000 0.00000000  1.0000000  0.00000000
# 9      0.0000000  0.0000000 0.45384961  0.2923480  0.25380243
# 10     0.3977816  0.0000000 0.27927906  0.1606384  0.16230097

}
}

\keyword{models}
