#' cforest {party}
#'
#' These defaults emulate cforest_unbiased() but allow customization.
#'
#' @param Y Outcome variable
#' @param X Covariate dataframe
#' @param newX Optional dataframe to predict the outcome
#' @param family "gaussian" for regression, "binomial" for binary
#'   classification
#' @param obsWeights Optional observation-level weights (supported but not tested)
#' @param id Optional id to group observations from the same unit (not used
#'   currently).
#' @param ntree Number of trees
#' @param mtry Number of randomly selected features per node
#' @param mincriterion See ?cforest_control
#' @param teststat See ?cforest_control
#' @param testtype See ?cforest_control
#' @param replace See ?cforest_control
#' @param fraction See ?cforest_control
#' @param ... Remaining arguments (unused)
SL.cforest <- function(Y, X, newX, family, obsWeights, id, ntree = 1000,
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

#
predict.SL.cforest <- function(object, newdata, ...) {
  .SL.require('party')
	pred <- predict(object = object$object, newdata = newdata)
	return(pred)
}