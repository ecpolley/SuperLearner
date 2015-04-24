## polymars{polspline}
# in the binomial case, drop the cv=5 selects model based on AIC
SL.polymars <- function(Y, X, newX, family, obsWeights, ...){
  .SL.require('polspline')
	if(family$family == "gaussian") { 
		fit.mars <- polspline::polymars(Y, X, weights = obsWeights)
 		pred <- predict(fit.mars, x = newX)
		fit <- list(object = fit.mars)
	}
	if(family$family == "binomial") {
		fit.mars <- polspline::polyclass(Y, X, cv = 5, weight = obsWeights)
		pred <- polspline::ppolyclass(cov = newX, fit = fit.mars)[, 2]
		fit <- list(fit = fit.mars)
	}
	out <- list(pred = pred, fit = fit)
	class(out$fit) <- c("SL.polymars")
	return(out)
}

predict.SL.polymars <- function(object, newdata, family, ...) {
  .SL.require('polspline')
	if(family$family=="gaussian"){ 
 		pred <- predict(object = object$object, x = newdata)
	}
	if(family$family=="binomial"){
		pred <- polspline::ppolyclass(cov=newdata, fit=object$fit)[, 2]
	}
	return(pred)
}