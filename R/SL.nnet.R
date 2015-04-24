# nnet{nnet}
# can change the size
# SL.nnet.3 <- function(..., size = 3) SL.nnet(..., size = size)

SL.nnet <- function(Y, X, newX, family, obsWeights, size = 2, ...){
  .SL.require('nnet')
	if(family$family == "gaussian") {
		fit.nnet <- nnet::nnet(x = X, y = Y, size = size, linout = TRUE, trace = FALSE, maxit = 500, weights = obsWeights)
	}
	if(family$family=="binomial") {
		fit.nnet <- nnet::nnet(x = X, y = Y, size = size, trace = FALSE, maxit = 500, linout = FALSE, weights = obsWeights)
	}
	pred <- predict(fit.nnet, newdata = newX, type = "raw")
	fit <- list(object = fit.nnet)
	out <- list(pred = pred, fit = fit)
	class(out$fit) <- c("SL.nnet")
	return(out)
}

predict.SL.nnet <- function(object, newdata,...) {
  .SL.require('nnet')
	pred <- predict(object$object, newdata = newdata, type = "raw")
	return(pred)
}

