#' @title Wrapper for lm
#' @description Wrapper for OLS via lm(), which may be faster than glm().
#'
#' @param Y Outcome variable
#' @param X Training dataframe
#' @param newX Test dataframe
#' @param family Gaussian or binomial
#' @param obsWeights Observation-level weights
#' @param model Whether to save model.matrix of data in fit object. Set to FALSE
#'   to save memory.
#' @param ... Any remaining arguments, not used.
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
#' sl = SuperLearner(Y, X, family = gaussian(),
#'                   SL.library = c("SL.mean", "SL.lm"))
#'
#' print(sl)
#'
#' @references
#'
#' Fox, J. (2015). Applied regression analysis and generalized linear models.
#' Sage Publications.
#'
#' @seealso \code{\link{predict.SL.lm}} \code{\link[stats]{lm}}
#'   \code{\link[stats]{predict.lm}}  \code{\link{SL.speedlm}}
#'
#' @export
SL.lm <- function(Y, X, newX, family, obsWeights, model = TRUE, ...) {

  # X must be a dataframe, not a matrix.
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  # qr element is needed in order to predict(), unless we extract coefficents.
  fit <- stats::lm(Y ~ ., data = X, weights = obsWeights, model = model)

  if (is.matrix(newX)) {
    newX = as.data.frame(newX)
  }

  pred <- predict(fit, newdata = newX, type = "response")

  # For binomial family restrict predicted probability to [0, 1].
  if (family$family == "binomial") {
    pred = pmin(pmax(pred, 0), 1)
  }

  fit <- list(object = fit, family = family)
  class(fit) <- "SL.lm"

  out <- list(pred = pred, fit = fit)

  return(out)
}

#' @title Prediction for SL.lm
#' @description Prediction for SL.lm
#'
#' @param object SL.lm object
#' @param newdata Dataframe to generate predictions
#' @param ... Unused additional arguments
#'
#' @seealso \code{\link{SL.lm}} \code{\link[stats]{lm}}
#'   \code{\link[stats]{predict.lm}}  \code{\link{SL.speedlm}}
#'
#' @export
predict.SL.lm <- function(object, newdata, ...) {

  # newdata must be a dataframe, not a matrix.
  if (is.matrix(newdata)) {
    newdata = as.data.frame(newdata)
  }

  pred <- predict(object = object$object, newdata = newdata, type = "response")

  # For binomial family restrict predicted probability to [0, 1].
  if (object$family$family == "binomial") {
    pred = pmin(pmax(pred, 0), 1)
  }

  pred
}