#' Wrapper for Bayesian GLM learner using `arm`
#'
#' Support Bayesian GLM via the \pkg{arm} package.
#'
#' @inheritParams SL.template
#' @inheritParams predict.SL.template
#' @inheritParams SL.glm

#' @export
SL.bayesglm <- function(Y, X, newX = X, family = gaussian(), obsWeights = NULL, ...){
  .SL.require('arm')
  fit.glm <- arm::bayesglm(Y ~ ., data = X, family = family, weights = obsWeights)
  pred <- predict(fit.glm, newdata = newX, type = "response")
  fit <- list(object = fit.glm)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.bayesglm")
  return(out)
}

#' @exportS3Method predict SL.bayesglm
#' @rdname SL.bayesglm
predict.SL.bayesglm <- function(object, newdata, ...) {
  .SL.require('arm')

  predict(object = object$object, newdata = newdata, type = "response")
}