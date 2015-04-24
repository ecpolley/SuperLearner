screen.glmnet <- function(Y, X, family, alpha = 1, minscreen = 2, nfolds = 10, nlambda = 100,  ...) {
  .SL.require('glmnet')
  if(!is.matrix(X)) {
    X <- model.matrix(~ -1 + ., X)
  }
  fitCV <- glmnet::cv.glmnet(x = X, y = Y, lambda = NULL, type.measure = 'deviance', nfolds = nfolds, family = family$family, alpha = alpha, nlambda = nlambda)
  whichVariable <- (as.numeric(coef(fitCV$glmnet.fit, s = fitCV$lambda.min))[-1] != 0)
  # the [-1] removes the intercept
  if (sum(whichVariable) < minscreen) {
      warning("fewer than minscreen variables passed the glmnet screen, increased lambda to allow minscreen variables")
	    sumCoef <- apply(as.matrix(fitCV$glmnet.fit$beta), 2, function(x) sum((x != 0)))
	    newCut <- which.max(sumCoef >= minscreen)
	    whichVariable <- (as.matrix(fitCV$glmnet.fit$beta)[, newCut] != 0)
  }
  return(whichVariable)
}