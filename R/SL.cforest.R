# cforest {party}
SL.cforest <- function(Y, X, newX, family, ...){
  .SL.require('party')
	if(family$family == "gaussian") {
		fit.cforest <- party::cforest(Y ~ ., data = data.frame(Y, X), controls = party::cforest_unbiased(ntree = 1000, mtry = max(floor(ncol(X)/3), 1)))
	}
	if(family$family == "binomial"){
		stop("Currently only works with gaussian data \ncforest can not return predicted probabilities")
	}
	pred <- predict(object = fit.cforest, newdata = newX)
	fit <- list(object = fit.cforest)
	out <- list(pred = pred, fit = fit)
	class(out$fit) <- c("SL.cforest")
	return(out)
}

# 
predict.SL.cforest <- function(object, newdata, ...) {
  .SL.require('party')
	pred <- predict(object = object$object, newdata = newdata)
	return(pred) 
}