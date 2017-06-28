#' @title Wrapper for glm
#' @description Wrapper for generalized linear models via glm().
#'
#' Note that for outcomes bounded by [0, 1] the binomial family can be used in
#' addition to gaussian.
#'
#' @param Y Outcome variable
#' @param X Training dataframe
#' @param newX Test dataframe
#' @param family Gaussian or binomial
#' @param obsWeights Observation-level weights
#' @param model Whether to save model.matrix of data in fit object. Set to FALSE
#' to save memory.
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
#'                   SL.library = c("SL.mean", "SL.glm"))
#'
#' print(sl)
#'
#' @references
#'
#' Fox, J. (2015). Applied regression analysis and generalized linear models.
#' Sage Publications.
#'
#' @seealso \code{\link{predict.SL.glm}} \code{\link[stats]{glm}}
#'   \code{\link[stats]{predict.glm}}  \code{\link{SL.speedglm}}
#'
#' @export
SL.glm <- function(Y, X, newX, family, obsWeights, model = TRUE, ...) {

  # X must be a dataframe, not a matrix.
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  fit.glm <- glm(Y ~ ., data = X, family = family, weights = obsWeights,
                 model = model)

  # newX must be a dataframe, not a matrix.
  if (is.matrix(newX)) {
    newX = as.data.frame(newX)
  }

  pred <- predict(fit.glm, newdata = newX, type = "response")
  fit <- list(object = fit.glm)
  class(fit) <- "SL.glm"
  out <- list(pred = pred, fit = fit)
  return(out)
}

#' @title Prediction for SL.glm
#' @description Prediction for SL.glm
#'
#' @param object SL.glm object
#' @param newdata Dataframe to generate predictions
#' @param ... Unused additional arguments
#'
#' @seealso \code{\link{SL.glm}} \code{\link[stats]{glm}}
#'   \code{\link[stats]{predict.glm}}  \code{\link{SL.speedglm}}
#'
#' @export
predict.SL.glm <- function(object, newdata, ...) {
  # newdata must be a dataframe, not a matrix.
  if (is.matrix(newdata)) {
    newdata = as.data.frame(newdata)
  }
  pred <- predict(object = object$object, newdata = newdata, type = "response")
  pred
}

SL.glm.interaction <- function(Y, X, newX, family, obsWeights, ...) {

  # X must be a dataframe, not a matrix.
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  fit.glm <- glm(Y ~ .^2, data = X, family = family, weights = obsWeights)

  # newX must be a dataframe, not a matrix.
  if (is.matrix(newX)) {
    newX = as.data.frame(newX)
  }

  pred <- predict(fit.glm, newdata = newX, type = "response")
  fit <- list(object = fit.glm)
  class(fit) <- "SL.glm"
  out <- list(pred = pred, fit = fit)
  return(out)
}
