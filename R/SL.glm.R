#' SL wrapper for `glm()`
#'
#' `SL.glm()` is a wrapper for generalized linear models (GLMs) fit with [glm()]. `SL.glm.interaction()` is the same but automatically includes all 2-way interactions between predictors. `SL.lm()` is a wrapper for linear models fit with [lm()]. `SL.mean()` is a wrapper for an intercept-only model.
#'
#' @inheritParams SL.template
#' @inheritParams predict.SL.template
#' @param model Whether to save `model.matrix` of data in fit object. Set to `FALSE`
#' to save memory.
#' @param ... Ignored.
#'
#' @details
#' Note that for outcomes bounded by \[0, 1\] the binomial family can be used in
#' addition to gaussian.
#'
#' @references
#' Fox, J. (2015). Applied regression analysis and generalized linear models.
#' Sage Publications.
#'
#' @seealso \code{\link{predict.SL.glm}} \code{\link[stats]{glm}}
#'   \code{\link[stats]{predict.glm}}  \code{\link{SL.speedglm}}
#'
#' @examples
#' data(Boston, package = "MASS")
#' Y = Boston$medv
#' # Remove outcome from covariate dataframe.
#' X = Boston[, -14]
#'
#' set.seed(1)
#'
#' sl <- SuperLearner(Y, X, family = gaussian(),
#'                    SL.library = c("SL.mean", "SL.glm",
#'                                   "SL.glm.interaction"))
#'
#' print(sl)

#' @export
SL.glm <- function(Y, X, newX = X, family = gaussian(), obsWeights = NULL, model = TRUE, ...) {
  if (is.null(obsWeights)) {
    obsWeights <- rep.int(1, length(Y))
  }

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

#' @export
#' @rdname SL.glm
SL.glm.interaction <- function(Y, X, newX = X, family = gaussian(), obsWeights = NULL, model = TRUE, ...) {
  if (is.null(obsWeights)) {
    obsWeights <- rep.int(1, length(Y))
  }

  # X must be a dataframe, not a matrix.
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  fit.glm <- glm(Y ~ .^2, data = X, family = family, weights = obsWeights,
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

#' @export
#' @rdname SL.glm
SL.lm <- function(Y, X, newX = X, family = gaussian(), obsWeights = NULL, model = TRUE, ...) {
  if (is.null(obsWeights)) {
    obsWeights <- rep.int(1, length(Y))
  }

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

#' @export
#' @rdname SL.glm
SL.mean <- function(Y, X, newX = X, family = gaussian(), obsWeights = NULL, model = TRUE, ...) {
  if (is.null(obsWeights)) {
    obsWeights <- rep.int(1, length(Y))
  }

  meanY <- weighted.mean(Y, w = obsWeights)

  pred <- rep.int(meanY, times = nrow(newX))
  fit <- list(object = meanY)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.mean")

  return(out)
}

#' @exportS3Method predict SL.glm
#' @rdname SL.glm
predict.SL.glm <- function(object, newdata, ...) {
  # newdata must be a dataframe, not a matrix.
  if (is.matrix(newdata)) {
    newdata = as.data.frame(newdata)
  }

  predict(object = object$object, newdata = newdata, type = "response")
}

#' @exportS3Method predict SL.lm
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

#' @exportS3Method predict SL.mean
predict.SL.mean <- function(object, newdata, ...) {
  rep.int(object$object, times = nrow(newdata))
}
