# earth {earth}
SL.earth <- function(Y, X, newX, family, obsWeights, id, degree = 2, penalty = 3, nk = max(21, 2*ncol(X) + 1), ...) {
  .SL.require('earth')
	if(family$family == "gaussian") {
		fit.earth <- earth::earth(x = X, y = Y, degree = degree, nk = nk, penalty = penalty)
	}
	if(family$family == "binomial") {
		fit.earth <- earth::earth(x = X, y = Y, degree = degree, nk = nk, penalty = penalty, glm = list(family = binomial))
	}
	pred <- predict(fit.earth, newdata = newX, type = "response")
	fit <- list(object = fit.earth)
	out <- list(pred = pred, fit = fit)
	class(out$fit) <- c("SL.earth")
	return(out)
}

# 
predict.SL.earth <- function(object, newdata,...) {
  .SL.require('earth')
	pred <- predict(object$object, newdata = newdata, type = "response")
	return(pred)
}