# randomForest{randomForest}

SL.randomForest <- function(Y, X, newX, family, mtry = ifelse(family$family=="gaussian", floor(sqrt(ncol(X))), max(floor(ncol(X)/3), 1)), ntree=1000, nodesize = ifelse(family$family=="gaussian", 5, 1), ...) {
	.SL.require('randomForest')
	if(family$family=="gaussian"){
		fit.rf <- randomForest::randomForest(Y ~ ., data = X, ntree = ntree, xtest = newX, keep.forest = TRUE, mtry = mtry, nodesize = nodesize)
		pred <- fit.rf$test$predicted
		fit <- list(object = fit.rf)
	}
	if(family$family=="binomial"){
		fit.rf <- randomForest::randomForest(y = as.factor(Y), x = X, ntree = ntree, xtest = newX, keep.forest = TRUE, mtry = mtry, nodesize = nodesize)
		pred <- fit.rf$test$votes[, 2]
		fit <- list(object = fit.rf)
	}
	out <- list(pred = pred, fit = fit)
	class(out$fit) <- c("SL.randomForest")
	return(out)
}

predict.SL.randomForest <- function(object, newdata, family, ...){
	.SL.require('randomForest')
	if(family$family=="gaussian"){
		pred <- predict(object$object, newdata = newdata, type = 'response')
	}
	if(family$family=="binomial"){
		pred <- predict(object$object, newdata = newdata, type = 'vote')[,2]
	}
	pred
}
