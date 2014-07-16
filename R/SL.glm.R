# generalized linear regression

SL.glm <- function(Y, X, newX, family, obsWeights, ...) {
  fit.glm <- glm(Y ~ ., data = X, family = family, weights = obsWeights)
  pred <- predict(fit.glm, newdata = newX, type="response")
  fit <- list(object = fit.glm)
  class(fit) <- "SL.glm"
  out <- list(pred = pred, fit = fit)
  return(out)
}

predict.SL.glm <- function(object, newdata, ...){
  pred <- predict(object = object$object, newdata = newdata, type = "response")
  pred
}

SL.glm.interaction <- function(Y, X, newX, family, obsWeights, ...) {
  fit.glm <- glm(Y ~ .^2, data = X, family = family, weights = obsWeights)
  pred <- predict(fit.glm, newdata = newX, type="response")
  fit <- list(object = fit.glm)
  class(fit) <- "SL.glm"
  out <- list(pred = pred, fit = fit)
  return(out)
}
