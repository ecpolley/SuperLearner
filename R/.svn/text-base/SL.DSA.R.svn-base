# DSA{DSA}
# may want to change:
# maxsize
# maxorderint
# maxsumofpow
# vfold

SL.DSA <- function(Y, X, newX, family, obsWeights, maxsize = 2*ncol(X), maxorderint = 2, maxsumofpow = 2, Dmove = TRUE, Smove = TRUE, vfold = 5, ...) {
  .SL.require('DSA')
	dsaweights <- matrix(obsWeights, nrow = (vfold +1), ncol = nrow(X), byrow = TRUE)
	fit.DSA <- DSA(Y ~ 1, data = data.frame(Y, X), family = family, maxsize = maxsize, maxorderint = maxorderint, maxsumofpow = maxsumofpow, Dmove = Dmove, Smove = Smove, vfold = vfold, weights = dsaweights)
	pred <- predict(fit.DSA, newdata = newX)
	if(family$family == "binomial") { pred <- 1 / (1 + exp(-pred))}
	fit <- list(object = fit.DSA)
	out <- list(pred = pred, fit = fit)
	class(out$fit) <- c("SL.DSA")
	return(out)
}

# 
predict.SL.DSA <- function(object, newdata, family, ...) {
  .SL.require('DSA')
	pred <- predict(object = object$object, newdata = newdata)
	if(family$family == "binomial"){ pred <- 1 / (1 + exp(-pred))}
	return(pred)
}