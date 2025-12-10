#' Wrapper for `speedglm`
#'
#' `speedglm::speedglm()` and `speedglm::speedlm()` are fast versions of `glm()` and `lm()`, respectively.
#'
#' @inheritParams SL.template
#' @inheritParams predict.SL.template
#' @inheritParams SL.glm
#' @param maxit Maximum number of iterations before stopping.
#' @param k numeric, the penalty per parameter to be used; the default k = 2 is
#'   the classical AIC.
#'
#' @references
#'
#' Enea, M. A. R. C. O. (2013). Fitting linear models and generalized linear
#' models with large data sets in R. Statistical Methods for the Analysis of
#' Large Datasets: book of short papers, 411-414.
#'
#' @seealso \code{\link{predict.SL.speedglm}} \code{\link[speedglm]{speedglm}}
#'   \code{\link[speedglm]{predict.speedglm}}

#' @export
SL.speedglm <- function(Y, X, newX = X, family = gaussian(), obsWeights = NULL,
                        maxit = 25, k = 2, ...) {
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

#' @export
#' @rdname SL.speedglm
SL.speedlm <- function(Y, X, newX = X, family = gaussian(), obsWeights = NULL, ...) {
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

#' @exportS3Method predict SL.speedglm
#' @rdname SL.speedglm
predict.SL.speedglm <- function(object, newdata, ...) {
  .SL.require("speedglm")

  # newdata must be a dataframe, not a matrix.
  if (is.matrix(newdata)) {
    newdata = as.data.frame(newdata)
  }

  predict(object = object$object, newdata = newdata, type = "response")
}

#' @exportS3Method predict SL.speedlm
#' @rdname SL.speedglm
predict.SL.speedlm <- predict.SL.speedglm