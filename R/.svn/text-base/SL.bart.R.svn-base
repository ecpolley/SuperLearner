# bart {BayesTree}
SL.bart <- function(Y, X, newX, family, ntree = 300, sigdf = 3, sigquant = 0.90, k = 2, power = 2, base = 0.95, binaryOffset = 0, ndpost = 1000, nskip = 100, ...) { 
  .SL.require('BayesTree')
	if(family$family == "gaussian") {
		fitBart <- bart(x.train = X, y.train = Y, x.test = newX, ntree = ntree, sigdf = sigdf, sigquant = sigquant, k = k, power = power, base = base, binaryOffset = binaryOffset, ndpost = ndpost, nskip = nskip, verbose = FALSE)
		pred  <- fitBart$yhat.test.mean
	}
	if(family$family == "binomial") {
		fitBart <- bart(x.train = X, y.train = as.factor(Y), x.test = newX, ntree = ntree, sigdf = sigdf, sigquant = sigquant, k = k, power = power, base = base, binaryOffset = binaryOffset, ndpost = ndpost, nskip = nskip, verbose = FALSE)
		pred <- pnorm(apply(fitBart$yhat.test, 2, mean))
	}
	fit <- list(object = fitBart)
	out <- list(pred = pred, fit = fit)
	class(out$fit) <- c("SL.bart")
	return(out)
}

# 
predict.SL.bart <- function(object, newdata, ...) {
	stop("no predict method currently available for bart")
}