#' @title Wrapper for speedglm
#' @description Speedglm is a fast version of glm()
#'
#' @param Y Outcome variable
#' @param X Training dataframe
#' @param newX Test dataframe
#' @param family Gaussian or binomial
#' @param obsWeights Observation-level weights
#' @param maxit Maximum number of iterations before stopping.
#' @param k numeric, the penalty per parameter to be used; the default k = 2 is
#'   the classical AIC.
#' @param ... Any remaining arguments, not used.
#'
#' @references
#'
#' Enea, M. A. R. C. O. (2013). Fitting linear models and generalized linear
#' models with large data sets in R. Statistical Methods for the Analysis of
#' Large Datasets: book of short papers, 411-414.
#'
#' @seealso \code{\link{predict.SL.speedglm}} \code{\link[speedglm]{speedglm}}
#'   \code{\link[speedglm]{predict.speedglm}}
#'
#' @export
SL.speedglm <- function(Y, X, newX, family, obsWeights,
                        maxit = 25,
                        k = 2,
                        ...) {
  .SL.require("speedglm")

  # X must be a dataframe, not a matrix.
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  fit <- speedglm::speedglm(Y ~ ., data = X, family = family,
                                weights = obsWeights,
                                maxit = maxit,
                                k = k)

  if (is.matrix(newX)) {
    newX = as.data.frame(newX)
  }

  pred <- predict(fit, newdata = newX, type = "response")

  fit <- list(object = fit)
  class(fit) <- "SL.speedglm"

  out <- list(pred = pred, fit = fit)

  return(out)
}

#' @title Prediction for SL.speedglm
#' @description Prediction for SL.speedglm
#'
#' @param object SL.speedglm object
#' @param newdata Dataframe to generate predictions
#' @param ... Unused additional arguments
#'
#' @seealso \code{\link{SL.speedglm}} \code{\link[speedglm]{speedglm}}
#'   \code{\link[speedglm]{predict.speedglm}}
#'
#' @export
predict.SL.speedglm <- function(object, newdata, ...) {
  .SL.require("speedglm")

  # newdata must be a dataframe, not a matrix.
  if (is.matrix(newdata)) {
    newdata = as.data.frame(newdata)
  }

  pred <- predict(object = object$object, newdata = newdata, type = "response")

  pred
}