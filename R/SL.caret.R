SL.caret <- function(Y, X, newX, family, obsWeights, method = "rf", tuneLength = 3, trControl = caret::trainControl(method = "cv", number = 10, verboseIter = TRUE), metric = ifelse(family$family == 'gaussian', 'RMSE', 'Accuracy'), ...) {
	.SL.require('caret')
  if (family$family == "gaussian") {
		fit.train <- caret::train(x = X, y = Y, weights = obsWeights, metric = metric, method = method, tuneLength = tuneLength, trControl = trControl)
		pred <- predict(fit.train, newdata = newX, type = "raw")
  }
  if (family$family == "binomial") {
	  # outcome must be factor, and have real labels
	  Y.f <- as.factor(Y)
	  levels(Y.f) <- c("A0", "A1")
	  fit.train <- caret::train(x = X, y = Y.f, weights = obsWeights, metric = metric, method = method, tuneLength = tuneLength, trControl = trControl)
	  pred <- predict(fit.train, newdata = newX, type = "prob")[, 2]
  }
    fit <- list(object = fit.train)
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.caret")
    return(out)
}

predict.SL.caret <- function(object, newdata, ...) {
  .SL.require('caret')
	if (object$object$modelType == "Regression") {
		pred <- predict(object$object, newdata = newdata, type = "raw")
	} else if (object$object$modelType == "Classification") {
		pred <- predict(object$object, newdata = newdata, type = "prob")[, 2]
	}
	return(pred)
}

# how to change to a different method:
SL.caret.rpart <- function(..., method = "rpart") {
	SL.caret(..., method = method)
}
