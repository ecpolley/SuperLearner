#' @title Wrapper for speedlm
#' @description Speedlm is a fast version of lm()
#'
#' @param Y Outcome variable
#' @param X Training dataframe
#' @param newX Test dataframe
#' @param family Gaussian or binomial
#' @param obsWeights Observation-level weights
#' @param ... Any remaining arguments, not used.
#'
#' @references
#'
#' Enea, M. A. R. C. O. (2013). Fitting linear models and generalized linear
#' models with large data sets in R. Statistical Methods for the Analysis of
#' Large Datasets: book of short papers, 411-414.
#'
#' @seealso \code{\link{predict.SL.speedlm}} \code{\link[speedglm]{speedlm}}
#'   \code{\link[speedglm]{predict.speedlm}}  \code{\link{SL.speedglm}}
#'
#' @export
SL.speedlm <- function(Y, X, newX, family, obsWeights,
                        ...) {
  .SL.require("speedglm")

  # X must be a dataframe, not a matrix.
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  fit <- speedglm::speedlm(Y ~ ., data = X, weights = obsWeights)

  if (is.matrix(newX)) {
    newX = as.data.frame(newX)
  }

  pred <- predict(fit, newdata = newX, type = "response")

  fit <- list(object = fit)
  class(fit) <- "SL.speedlm"

  out <- list(pred = pred, fit = fit)

  return(out)
}

#' @title Prediction for SL.speedlm
#' @description Prediction for SL.speedlm, a fast lm()
#'
#' @param object SL.speedlm object
#' @param newdata Dataframe to generate predictions
#' @param ... Unused additional arguments
#'
#' @seealso \code{\link{SL.speedlm}} \code{\link[speedglm]{speedlm}}
#'   \code{\link[speedglm]{predict.speedlm}}  \code{\link{SL.speedglm}}
#'
#' @export
predict.SL.speedlm <- function(object, newdata, ...){
  .SL.require("speedglm")

  # newdata must be a dataframe, not a matrix.
  if (is.matrix(newdata)) {
    newdata = as.data.frame(newdata)
  }

  pred <- predict(object = object$object, newdata = newdata, type = "response")

  pred
}