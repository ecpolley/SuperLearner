#' SL wrapper for `party::cforest()`
#'
#' These defaults emulate `party::cforest_unbiased()` but allow customization.
#'
#' @inheritParams SL.template
#' @inheritParams predict.SL.template
#' @inheritParams SL.glm
#' @param ntree Number of trees.
#' @param mtry Number of randomly selected features per node.
#' @param mincriterion,teststat,testtype,replace,fraction Arguments passed to `party::cforest_control()`.

#' @export
SL.cforest <- function(Y, X, newX = X, family = gaussian(), obsWeights = NULL, ntree = 1000,
                       mtry = max(floor(ncol(X) / 3), 1), mincriterion = 0,
                       teststat = "quad", testtype = "Univ", replace = F,
                       fraction = 0.632, ...) {
  .SL.require('party')
	controls = party::cforest_control(ntree = ntree, mtry = mtry,
	                        mincriterion = mincriterion, teststat = teststat,
	                        testtype = testtype, replace = replace, fraction = fraction)

	# We can use the same estimation code for classification and regression.
	fit.cforest <- party::cforest(Y ~ ., data = data.frame(Y, X), controls = controls,
	                              weights = obsWeights)
	pred <- predict(object = fit.cforest, newdata = newX)
	fit <- list(object = fit.cforest)
	out <- list(pred = pred, fit = fit)
	class(out$fit) <- c("SL.cforest")
	return(out)
}

#' @exportS3Method predict SL.cforest
#' @rdname SL.cforest
predict.SL.cforest <- function(object, newdata, ...) {
  .SL.require('party')

  predict(object = object$object, newdata = newdata)
}