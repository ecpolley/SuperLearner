#' @title SL wrapper for MASS:qda
#' @description Quadratic discriminant analysis, used for classification.
#'
#' @param Y Outcome variable
#' @param X Training dataframe
#' @param newX Test dataframe
#' @param family Binomial only, cannot be used for regression.
#' @param obsWeights Observation-level weights
#' @param id Not supported.
#' @param verbose If TRUE, display additional output during execution.
#
# Algorithm-specific arguments:
#
#' @param prior the prior probabilities of class membership. If unspecified, the
#'   class proportions for the training set are used. If present, the
#'   probabilities should be specified in the order of the factor levels.
#' @param method "moment" for standard estimators of the mean and variance,
#'   "mle" for MLEs, "mve" to use cov.mve, or "t" for robust estimates based on
#'   a t distribution.
#' @param tol tolerance
#' @param CV If true, returns results (classes and posterior probabilities) for
#'   leave-one-out cross-validation. Note that if the prior is estimated, the
#'   proportions in the whole dataset are used.
#' @param nu degrees of freedom for method = "t".
#' @param ... Any additional arguments, not currently used.
#'
#' @examples
#'
#' data(Boston, package = "MASS")
#' Y = as.numeric(Boston$medv > 23)
#' # Remove outcome from covariate dataframe.
#' X = Boston[, -14]
#'
#' set.seed(1)
#'
#' # Use only 2 CV folds to speed up example.
#' sl = SuperLearner(Y, X, family = binomial(), cvControl = list(V = 2),
#'                  SL.library = c("SL.mean", "SL.qda"))
#' sl
#'
#' pred = predict(sl, X)
#' summary(pred$pred)
#'
#'
#' @references
#'
#' James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An Introduction
#' to Statistical Learning (Vol. 6). New York: Springer. Section 4.4.
#'
#' @seealso \code{\link{predict.SL.qda}} \code{\link[MASS]{qda}}
#'   \code{\link[MASS]{predict.qda}} \code{\link{SL.lda}}
#'
#' @importFrom utils capture.output
#'
#' @export
SL.qda =
  function(Y, X, newX, family, obsWeights = rep(1, nrow(X)),
           verbose = F, id = NULL,
           prior = as.vector(prop.table(table(Y))),
           method = "mle",
           tol = 1.0e-4,
           CV = F,
           nu = 5,
           ...) {

  .SL.require("MASS")

  if (family$family != "binomial") {
    stop("SL.qda only supports binomial outcomes.")
  }

  if (!is.factor(Y)) {
    Y = as.factor(Y)
  }

  # X can be a matrix or dataframe.
  # If method = "t" this will print a lot of unnecessary output, so capture it.
  capture.output({
    fit = MASS::qda(x = X,
                    grouping = Y,
                    prior = prior,
                    method = method,
                    tol = tol,
                    CV = CV,
                    nu = nu)
  })

  pred = predict(fit, newX)$posterior

  # $posterior is a two-column matrix; we want P(Y = 1 | X).
  pred = pred[, "1"]

  fit = list(object = fit, verbose = verbose)
  class(fit) = "SL.qda"
  out = list(pred = pred, fit = fit)
  return(out)
}

#' @title Prediction wrapper for SL.qda
#'
#' @description Prediction wrapper for SL.qda
#'
#' @param object SL.lda object
#' @param newdata Dataframe to generate predictions
#' @param prior The prior probabilities of the classes, by default the
#'   proportions in the training set or what was set in the call to lda.
#' @param dimen the dimension of the space to be used. If this is less than
#'   min(p, ng-1), only the first dimen discriminant components are used (except
#'   for method="predictive"), and only those dimensions are returned in x.
#' @param method This determines how the parameter estimation is handled. With
#'   "plug-in" (the default) the usual unbiased parameter estimates are used and
#'   assumed to be correct. With "debiased" an unbiased estimator of the log
#'   posterior probabilities is used, and with "predictive" the parameter
#'   estimates are integrated out using a vague prior.
#' @param ... Unused additional arguments
#'
#' @seealso \code{\link{SL.qda}} \code{\link[MASS]{qda}}
#'   \code{\link[MASS]{predict.qda}}
#'
#' @export
predict.SL.qda <- function(object, newdata,
                           prior = object$object$prior,
                           dimen = NULL,
                           method = "plug-in",
                           ...) {
  .SL.require("MASS")

  pred = predict(object$object, newdata,
                 prior = prior,
                 dimen = dimen,
                 method = method)$posterior

  # $posterior is a two-column matrix.
  pred = pred[, "1"]

  return(pred)
}
