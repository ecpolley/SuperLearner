## lm.ridge{MASS}
# may want to change range lambda searches over
# will only work with guassian

#' @export
SL.ridge <- function(Y, X, newX = X, family = gaussian(), lambda = seq(1, 20, .1), ...) {
	.SL.require('MASS')
	if(family$family=="binomial"){
		stop("Currently only works with gaussian data")
	}
	fit.ridge <- MASS::lm.ridge(Y ~ ., data = X, lambda = lambda)
	bestCoef <- as.matrix(coef(fit.ridge)[which.min(fit.ridge$GCV), ])
	m <- dim(newX)[1]
	newx.ridge <- cbind(rep(1, m), as.matrix(newX))
	pred <- newx.ridge %*% bestCoef
	fit <- list(bestCoef = bestCoef)
	out <- list(pred = pred, fit = fit)
	class(out$fit) <- c("SL.ridge")
	return(out)
}

#' @exportS3Method predict SL.ridge
predict.SL.ridge <- function(object, newdata, ...){
	m <- nrow(newdata)
	newx.ridge <- as.matrix(cbind(rep.int(1, m), newdata))

	newx.ridge %*% object$bestCoef
}