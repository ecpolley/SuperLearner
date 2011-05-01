screen.glmnet <- function(Y, X, family, alpha = 1, minscreen = 2, nfolds = 10, nlambda = 100,  ...) {
  .SL.require('glmnet')
  if(!is.matrix(X)) {
    X <- model.matrix(~ -1 + ., X)
    newX <- model.matrix(~ -1 + ., newX)
  }
  fitCV <- cv.glmnet(x = X, y = Y, lambda = NULL, type.measure = 'deviance', nfolds = nfolds, family = family$family, alpha = alpha, nlambda = nlambda)
  whichVariable <- (fit.first$beta[, which.min(cv.net.fit$cv)] != 0)
  if (sum(whichVariable) < minscreen) {
      warning("fewer than minscreen variables passed the glmnet screen, increased lambda to allow minscreen variables")
	sumCoef <- apply(fit.first$beta, 2, function(x) sum((x != 0)))
	newCut <- which.max(sumCoef >= minscreen)
	whichVariable <- (fit.first$beta[, newCut] != 0)
  }
  return(whichVariable)
  # whichVariable is a logical vector,
  # TRUE indicates variable will be used
  whichVariable <- rep(TRUE, ncol(X))
  return(whichVariable)
}