# rpart {rpart}
#' @export
SL.rpart <- function(Y, X, newX = X, family = gaussian(), obsWeights = NULL,
                     cp = 0.01, minsplit = 20, xval = 0L,
                     maxdepth = 30, minbucket = round(minsplit/3), ...) {
  .SL.require('rpart')
	if (family$family == "gaussian"){
		fit.rpart <- rpart::rpart(Y~., data = data.frame(Y, X), control = rpart::rpart.control(cp = cp, minsplit = minsplit, xval = xval, maxdepth = maxdepth, minbucket = minbucket), method = "anova", weights = obsWeights)
		pred <- predict(fit.rpart, newdata = newX)
	}
	if(family$family == "binomial") {
		fit.rpart <- rpart::rpart(Y ~ ., data = data.frame(Y, X), control = rpart::rpart.control(cp = cp, minsplit = minsplit, xval = xval, maxdepth = maxdepth, minbucket = minbucket), method = "class", weights = obsWeights)
		pred <- predict(fit.rpart, newdata = newX)[, 2]
	}
	fit <- list(object = fit.rpart)
	out <- list(pred = pred, fit = fit)
	class(out$fit) <- c("SL.rpart")
	return(out)
}

#' @export
SL.rpartPrune <- function(Y, X, newX = X, family = gaussian(), obsWeights = NULL,
                          cp = 0.001, minsplit = 20, xval = 10, maxdepth = 20, minbucket = 5, ...) {
  .SL.require("rpart")
  if (family$family == "gaussian") {
    fit.rpart <- rpart::rpart(Y ~ ., data = data.frame(Y, X), control = rpart::rpart.control(cp = cp, minsplit = minsplit, xval = xval, maxdepth = maxdepth, minbucket = minbucket), method = "anova", weights = obsWeights)
    CP <- fit.rpart$cptable[which.min(fit.rpart$cptable[, "xerror"]), "CP"]
    fitPrune <- rpart::prune(fit.rpart, cp = CP)
    pred <- predict(fitPrune, newdata = newX)
  }
  if (family$family == "binomial") {
    fit.rpart <- rpart::rpart(Y ~ ., data = data.frame(Y, X), control = rpart::rpart.control(cp = cp, minsplit = minsplit, xval = xval, maxdepth = maxdepth, minbucket = minbucket), method = "class", weights = obsWeights)
    CP <- fit.rpart$cptable[which.min(fit.rpart$cptable[, "xerror"]), "CP"]
    fitPrune <- rpart::prune(fit.rpart, cp = CP)
    pred <- predict(fitPrune, newdata = newX)[, 2]
  }
  fit <- list(object = fitPrune, fit = fit.rpart, cp = CP)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.rpart")
  return(out)
}

#' @exportS3Method predict SL.rpart
predict.SL.rpart <- function(object, newdata, ...) {
	.SL.require('rpart')
  pred <- predict(object$object, newdata = newdata)

	if (NCOL(pred) > 1L) {
    pred <- pred[, 2]
	}

	return(pred)
}

