# rpart {rpart}
SL.rpart <- function(Y, X, newX, family, obsWeights, cp = 0.01, minsplit = 20, xval = 10, maxdepth = 30, minbucket = round(minsplit/3), ...) {
  .SL.require('rpart')
	if(family$family == "gaussian"){
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

# 
predict.SL.rpart <- function(object, newdata, family, ...) {
	.SL.require('rpart')
	if(family$family=="gaussian") { 
	  pred <- predict(object$object, newdata = newdata)
  }
  if(family$family=="binomial") {
    pred <- predict(object$object, newdata = newdata)[, 2]
  }
	return(pred)
}

