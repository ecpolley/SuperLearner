# cforest {party}
# These defaults emulate cforest_unbiased() but allow customization.
#' TODO: document all parameters.
SL.cforest <- function(Y, X, newX, family, ntree = 1000,
                       mtry = max(floor(ncol(X) / 3), 1), mincriterion = 0,
                       teststat = "quad", testtype = "Univ", replace = F,
                       fraction = 0.632, ...) {
  .SL.require('party')
	controls = party::cforest_control(ntree = ntree, mtry = mtry,
	                        mincriterion = mincriterion, teststat = teststat,
	                        testtype = testtype, replace = replace, fraction = fraction)

	# We can use the same estimation code for classification and regression.
	fit.cforest <- party::cforest(Y ~ ., data = data.frame(Y, X), controls = controls)
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