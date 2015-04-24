# bagging {ipred}
# 

SL.ipredbagg <- function(Y, X, newX, family, nbagg = 100, control = rpart::rpart.control(xval = 0, maxsurrogate = 0, minsplit = 20, cp = 0.01, maxdepth = 30), ...){
	.SL.require('ipred')
	if(family$family == "gaussian"){
		fit.bag <- ipred::ipredbagg(y=Y, X = X, nbagg = nbagg, control = control)
		pred <- predict(fit.bag, newdata = newX, aggregation = "average")
	}
	if(family$family == "binomial"){
		fit.bag <- ipred::ipredbagg(y = as.factor(Y), X = X, nbagg = nbagg, control = control)
		pred <- predict(fit.bag, newdata = newX, type = "prob", aggregation = "average")[, 2]
	}
	fit <- list(object = fit.bag)
	out <- list(pred = pred, fit = fit)
	class(out$fit) <- c("SL.ipredbagg")
	return(out)
}

# 
predict.SL.ipredbagg <- function(object, newdata, family, X=NULL, Y=NULL,...) {
  .SL.require('ipred')
	if(family$family=="gaussian"){
		pred <- predict(object = object$object, newdata = newdata, aggregation = "average")
	}
	if(family$family=="binomial"){
		pred <- predict(object = object$object, newdata = newdata, type = "prob", aggregation = "average")[, 2] 
	}
	return(pred)
}

