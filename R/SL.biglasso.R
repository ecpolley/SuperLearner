#' @title SL wrapper for biglasso
#' @description SL wrapper for biglasso
#'
#' @param Y Outcome variable
#' @param X Training dataframe
#' @param newX Test dataframe
#' @param family Gaussian or binomial
#' @param obsWeights Observation-level weights
#' @param penalty The penalty to be applied to the model. Either "lasso"
#'   (default), "ridge", or "enet" (elastic net).
#' @param alg.logistic The algorithm used in logistic regression. If "Newton"
#'   then the exact hessian is used (default); if "MM" then a
#'   majorization-minimization algorithm is used to set an upper-bound on the
#'   hessian matrix. This can be faster, particularly in data-larger-than-RAM
#'   case.
#' @param screen "SSR" (default) is the sequential strong rule; "SEDPP" is the
#'   (sequential) EDPP rule. "SSR-BEDPP", "SSR-Dome", and "SSR-Slores" are our
#'   newly proposed screening rules which combine the strong rule with a safe
#'   rule (BEDPP, Dome test, or Slores rule). Among the three, the first two are
#'   for lasso-penalized linear regression, and the last one is for
#'   lasso-penalized logistic regression. "None" is to not apply a screening
#'   rule.
#' @param alpha The elastic-net mixing parameter that controls the relative
#'   contribution from the lasso (l1) and the ridge (l2) penalty.
#' @param nlambda The number of lambda values to check.  Default is 100.
#' @param eval.metric The evaluation metric for the cross-validated error and
#'   for choosing optimal \code{lambda}. "default" for linear regression is MSE
#'   (mean squared error), for logistic regression is misclassification error.
#'   "MAPE", for linear regression only, is the Mean Absolute Percentage Error.
#' @param ncores The number of cores to use for parallel execution across a
#'   cluster created by the \code{parallel} package.
#' @param nfolds The number of cross-validation folds.  Default is 5.
#' @param ... Any additional arguments, not currently used.
#'
#' @importFrom biglasso cv.biglasso
#' @importFrom bigmemory as.big.matrix
#'
#' @examples
#'
#' data(Boston, package = "MASS")
#' Y = Boston$medv
#' # Remove outcome from covariate dataframe.
#' X = Boston[, -14]
#'
#' set.seed(1)
#'
#' # Sample rows to speed up example.
#' row_subset = sample(nrow(X), 30)
#'
#' # Subset rows and columns & use only 2 folds to speed up example.
#' sl = SuperLearner(Y[row_subset], X[row_subset, 1:2, drop = FALSE],
#'                   family = gaussian(), cvControl = list(V = 2),
#'                   SL.library = "SL.biglasso")
#' sl
#'
#' pred = predict(sl, X)
#' summary(pred$pred)
#'
#' @references
#'
#' Zeng Y, Breheny P (2017). biglasso: Extending Lasso Model Fitting to Big
#' Data. https://CRAN.R-project.org/package=biglasso.
#'
#' @seealso \code{\link{predict.SL.biglasso}} \code{\link[biglasso]{biglasso}}
#'   \code{\link[biglasso]{cv.biglasso}}
#'   \code{\link[biglasso]{predict.biglasso}} \code{\link{SL.glmnet}}
#'
#' @export
SL.biglasso <-
  function(Y, X, newX, family,
           obsWeights,
           penalty = "lasso",
           alg.logistic = "Newton",
           screen = "SSR",
           alpha = 1,
           nlambda = 100,
           eval.metric = "default",
           ncores = 1,
           nfolds = 5,
           ...) {
  .SL.require("biglasso")
  .SL.require("bigmemory")  

  # If binomial, biglasso still wants Y to be a numeric.

  if (!is.matrix(X)) {
    X = model.matrix(~ ., X)
    # Remove intercept that was added.
    X = X[, -1]
  }

  # This will give a warning if X is only a single covariate.
  X = bigmemory::as.big.matrix(X)

  fit = biglasso::cv.biglasso(X, Y, family = family$family,
                           penalty = penalty,
                           alg.logistic = alg.logistic,
                           screen = screen,
                           eval.metric = eval.metric,
                           ncores = ncores,
                           alpha = alpha,
                           nfolds = nfolds,
                           nlambda = nlambda)

  if (!is.matrix(newX)) {
    newX = model.matrix(~ ., newX)
    # Remove intercept that was added.
    newX = newX[, -1]
  }

  newX = bigmemory::as.big.matrix(newX)
  pred <- predict(fit, newX, type = "response")

  fit <- list(object = fit)
  class(fit) <- c("SL.biglasso")

  # Explicitly convert pred to vector as pred may be a Matrix::dgeMatrix
  out <- list(pred = as.vector(pred), fit = fit)
  return(out)
}

#' @title Prediction wrapper for SL.biglasso
#'
#' @description Prediction wrapper for SL.biglasso objects.
#'
#' @param object SL.kernlab object
#' @param newdata Dataframe to generate predictions
#' @param ... Unused additional arguments
#'
#' @importFrom bigmemory as.big.matrix
#'
#' @seealso \code{\link{SL.biglasso}} \code{\link[biglasso]{biglasso}}
#'   \code{\link[biglasso]{predict.biglasso}}
#'
#' @export
predict.SL.biglasso <- function(object, newdata,
                              ...) {
  .SL.require("biglasso")
  .SL.require("bigmemory")

  if (!is.matrix(newdata)) {
    newdata = model.matrix(~ ., newdata)
    # Remove intercept that was added.
    newdata = newdata[, -1]
  }

  newdata = bigmemory::as.big.matrix(newdata)

  # Binomial and gaussian prediction is the same.
  pred <- predict(object$object, newdata, type = "response")

  # Explicitly convert to vector as pred may be a Matrix::dgeMatrix
  as.vector(pred)
}
