# 
SL.template <- function(Y, X, newX, family, obsWeights, id, ...) {
		if(family$family == "gaussian") {
		# insert estimation and prediction function 
		}
		if(family$family == "binomial") {
		# insert estimation and prediction function 
		}
		# pred returns predicted responses (on the scale of the outcome)
		pred <- numeric()
		# fit returns all objects needed for predict.SL.template
		# fit <- list(object = )
		fit <- vector("list", length=0)
		class(fit) <- c("SL.template")
		out <- list(pred = pred, fit = fit)
		return(out)
}

# 
predict.SL.template <- function(object, newdata, family, X = NULL, Y = NULL,...) {
	# insert prediction function
	pred <- numeric()
	return(pred)
}

write.SL.template <- function(file = '', ...) {
  cat('SL.template <- function(Y, X, newX, family, obsWeights, id, ...) {\n  # load required packages\n  # require(\'pkg\')\n  if(family$family == \'gaussian\') {\n  \n  }\n  if(family$family == \'binomial\') {\n  \n  }\n  # pred is the predicted responses for newX (on the scale of the outcome)\n  pred <- numeric()\n  # fit returns all objects needed for predict.SL.template\n  fit <- list(object = )\n  # declare class of fit for predict.SL.template\n  class(fit) <- \'SL.template\'\n  # return a list with pred and fit\n  out <- list(pred = pred, fit = fit)\n  return(out)\n}', file = file, ...)
}