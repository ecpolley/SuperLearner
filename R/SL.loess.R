# loess {stats}
# l.family can be either 'gaussian' (least squares) or 'symmetric' (M-estimator with Tukey's biweight)

SL.loess <- function(Y, X, newX, family, obsWeights, span = 0.75, l.family = "gaussian", ...) {
	if(family$family == "gaussian") {
		fit.loess <- loess(as.formula(paste("Y~", names(X))), data = X, family = l.family, span = span, control = loess.control(surface = "direct"), weights = obsWeights)
	}
	if(family$family == "binomial") {
				stop('family = binomial() not currently implemented for SL.loess')
	}
	pred <- predict(fit.loess, newdata = newX)
	fit <- list(object = fit.loess)
	out <- list(pred = pred,fit = fit)
	class(out$fit) <- c("SL.loess")
	return(out)
}

# 
predict.SL.loess <- function(object, newdata, ...) {
	pred <- predict(object = object$object, newdata = newdata)
	return(pred)
}

