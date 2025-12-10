#' SL wrapper for `earth::earth()`
#'
#' @inheritParams SL.template
#' @inheritParams predict.SL.template
#' @inheritParams SL.glm
#' @param degree,penalty,nk,pmethod,nfold,ncross,minspan,endspan Arguments passed to `earth::earth()`.

#' @export
SL.earth <- function(Y, X, newX = X, family = gaussian(), degree = 2, penalty = 3,
                     nk = max(21, 2*ncol(X) + 1), pmethod = "backward", nfold = 0,
                     ncross = 1, minspan = 0, endspan = 0, ...) {
  .SL.require('earth')

	if(family$family == "gaussian") {
		fit.earth <- earth::earth(x = X, y = Y, degree = degree, nk = nk, penalty = penalty,
		                          pmethod = pmethod, nfold = nfold, ncross = ncross,
		                          minspan = minspan, endspan = endspan)
	}
	if(family$family == "binomial") {
		fit.earth <- earth::earth(x = X, y = Y, degree = degree, nk = nk, penalty = penalty,
		                          pmethod = pmethod, nfold = nfold, ncross = ncross,
		                          minspan = minspan, endspan = endspan, glm = list(family = binomial))
	}
	pred <- predict(fit.earth, newdata = newX, type = "response")
	fit <- list(object = fit.earth)
	out <- list(pred = pred, fit = fit)
	class(out$fit) <- c("SL.earth")
	return(out)
}

#' @exportS3Method predict SL.earth
#' @rdname SL.earth
predict.SL.earth <- function(object, newdata,...) {
  .SL.require('earth')

  predict(object$object, newdata = newdata, type = "response")
}
