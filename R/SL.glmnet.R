SL.glmnet <- function(Y, X, newX, family, obsWeights, id, alpha = 1, nfolds = 10, nlambda = 100, useMin = TRUE, ...) {
  .SL.require('glmnet')
  # X must be a matrix, should we use model.matrix or as.matrix
  if(!is.matrix(X)) {
    X <- model.matrix(~ -1 + ., X)
    newX <- model.matrix(~ -1 + ., newX)
  }
  # now use CV to find lambda
  fitCV <- glmnet::cv.glmnet(x = X, y = Y, weights = obsWeights, lambda = NULL, type.measure = 'deviance', nfolds = nfolds, family = family$family, alpha = alpha, nlambda = nlambda)
  # two options for lambda, fitCV$lambda.min and fitCV$lambda.1se
  pred <- predict(fitCV$glmnet.fit, newx = newX, s = ifelse(useMin, fitCV$lambda.min, fitCV$lambda.1se), type = 'response')
  fit <- list(object = fitCV, useMin = useMin)
  class(fit) <- 'SL.glmnet'
  out <- list(pred = pred, fit = fit)
  return(out)
}

predict.SL.glmnet <- function(object, newdata, ...) {
  if(!is.matrix(newdata)) {
    newdata <- model.matrix(~ -1 + ., newdata)
  }
  pred <- predict(object$object$glmnet.fit, newx = newdata, s = ifelse(object$useMin, object$object$lambda.min, object$object$lambda.1se), type = 'response')
  return(pred)
}