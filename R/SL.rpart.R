# rpart {rpart}
SL.rpart <- function(Y, X, newX, family, obsWeights, cp = 0.01, minsplit = 20, xval = 10, maxdepth = 30, ...) {
  .SL.require('rpart')
	if(family$family == "gaussian"){
		fit.rpart <- rpart(Y~., data = data.frame(Y, X), control = rpart.control(cp = cp, minsplit = minsplit, xval = xval, maxdepth = maxdepth), method = "anova", weights = obsWeights)
		pred <- predict(fit.rpart, newdata = newX)
	}
	if(family$family == "binomial") {
		fit.rpart <- rpart(Y ~ ., data = data.frame(Y, X), control = rpart.control(cp = cp, minsplit = minsplit, xval = xval, maxdepth = maxdepth), method = "class", weights = obsWeights)
		pred <- predict(fit.rpart, newdata = newX)[, 2]
	}
	fit <- list(object = fit.rpart)
	out <- list(pred = pred, fit = fit)
	class(out$fit) <- c("SL.rpart")
	return(out)
}

# 
predict.SL.rpart <- function(object, newdata, ...) {
  .SL.require('rpart')
	pred <- predict(object, newdata = newdata)
	return(pred)
}

