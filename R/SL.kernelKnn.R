#' @title SL wrapper for KernelKNN
#'
#' @description Wrapper for a configurable implementation of k-nearest
#'   neighbors. Supports both binomial and gaussian outcome distributions.
#' @param Y Outcome variable
#' @param X Training dataframe
#' @param newX Test dataframe
#' @param family Gaussian or binomial
#' @param k Number of nearest neighbors to use
#' @param method Distance method, can be 'euclidean' (default), 'manhattan',
#'   'chebyshev', 'canberra', 'braycurtis', 'pearson_correlation',
#'   'simple_matching_coefficient', 'minkowski' (by default the order 'p' of the
#'   minkowski parameter equals k), 'hamming', 'mahalanobis',
#'   'jaccard_coefficient', 'Rao_coefficient'
#' @param weights_function Weighting method for combining the nearest neighbors.
#'   Can be 'uniform' (default), 'triangular', 'epanechnikov', 'biweight',
#'   'triweight', 'tricube', 'gaussian', 'cosine', 'logistic', 'gaussianSimple',
#'   'silverman', 'inverse', 'exponential'.
#' @param extrema if TRUE then the minimum and maximum values from the
#'   k-nearest-neighbors will be removed (can be thought as outlier removal).
#' @param h the bandwidth, applicable if the weights_function is not NULL.
#'   Defaults to 1.0.
#' @param ... Any additional parameters, not currently passed through.
#' @return List with predictions and the original training data &
#'   hyperparameters.
#'
#' @examples
#'
#' # Load a test dataset.
#' data(PimaIndiansDiabetes2, package = "mlbench")
#'
#' data = PimaIndiansDiabetes2
#'
#' # Omit observations with missing data.
#' data = na.omit(data)
#'
#' Y_bin = as.numeric(data$diabetes)
#' X = subset(data, select = -diabetes)
#'
#' set.seed(1)
#'
#' sl = SuperLearner(Y_bin, X, family = binomial(),
#'                  SL.library = c("SL.mean", "SL.kernelKnn"))
#' sl
#'
#' @export
SL.kernelKnn = function(Y, X, newX, family,
                        k = 10,
                        method = "euclidean",
                        weights_function = NULL,
                        extrema = F,
                        h = 1,
                        ...) {
  .SL.require("KernelKnn")

  if (family$family != "gaussian" && min(Y) == 0) {
    # Make sure that Y starts at 1 rather than 0.
    Y = Y + 1
  }

  if (family$family == "gaussian") {
    levels = NULL
  } else {
    levels = unique(Y)
  }

  pred = KernelKnn::KernelKnn(data = X, TEST_data = newX, y = Y, k = k,
                              method = method, h = h,
                              weights_function = weights_function,
                              regression = family$family == "gaussian",
                              Levels = levels)

  if (family$family == "binomial") {
    # Pred is a two-column matrix, where column 1 is Pr(Y = 0), 2 is Pr(Y = 1)
    pred = pred[, 2]
  }

  # Save configuration plus original X and Y data to the fit object.
  fit = list(k = k, method = method, weights_function = weights_function,
              extrema = extrema, h = h,
              X = X, Y = Y, family = family)

  out = list(pred = pred, fit = fit)

  class(out$fit) = "SL.kernelKnn"
  return(out)
}

#' Prediction for SL.kernelKnn
#' @param object SL.kernelKnn object
#' @param newdata Dataframe to generate predictions
#' @param ... Unused additional arguments
#' @export
predict.SL.kernelKnn <- function(object, newdata, ...) {
  .SL.require("KernelKnn")

  if (object$family$family == "gaussian") {
    levels = NULL
  } else {
    levels = unique(object$Y)
  }

  pred = KernelKnn::KernelKnn(data = object$X,
                              TEST_data = newdata,
                              y = object$Y,
                              k = object$k,
                              method = object$method,
                              h = object$h,
                              weights_function = object$weights_function,
                              regression = object$family$family == "gaussian",
                              Levels = levels)

  if (object$family$family == "binomial") {
    # Pred is a two-column matrix, where column 1 is Pr(Y = 0), 2 is Pr(Y = 1)
    pred = pred[, 2]
  }

  return(pred)
}
