# loess {stats}
# l.family can be either 'gaussian' (least squares) or 'symmetric' (M-estimator with Tukey's biweight)

#' @export
SL.loess <- function(Y, X, newX = X, family = gaussian(), obsWeights = NULL, span = 0.75, l.family = "gaussian", ...) {
	if(family$family == "gaussian") {
		fit.loess <- loess(Y ~ ., data = X, family = l.family, span = span,
		                   control = loess.control(surface = "direct"), weights = obsWeights)
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

#' @exportS3Method predict SL.loess
predict.SL.loess <- function(object, newdata, ...) {
	predict(object = object$object, newdata = newdata)
}

