# bayesglm{arm}
# Bayesian generalized linear regression

SL.bayesglm <- function(Y, X, newX, family, obsWeights, ...){
  .SL.require('arm')
  fit.glm <- arm::bayesglm(Y ~ ., data = X, family = family, weights = obsWeights)
  pred <- predict(fit.glm, newdata = newX, type = "response")
  fit <- list(object = fit.glm)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.bayesglm")
  return(out)
}

predict.SL.bayesglm <- function(object, newdata, ...){
  .SL.require('arm')
  pred <- predict(object = object$object, newdata = newdata, type = "response")
  return(pred)
}