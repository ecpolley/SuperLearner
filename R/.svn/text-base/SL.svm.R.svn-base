# svm{e1071}
# two types for regression: "nu-regression" and "eps-regression".
# two types for classification: "nu-classification" and "C-classification"
# many other tuning parameters to consider
SL.svm <- function(Y, X, newX, family, type.reg = "nu-regression", type.class = "nu-classification", nu = 0.5, ...) {
  .SL.require('e1071')
	if(family$family == "gaussian") {
		fit.svm <- svm(y = Y, x = X, nu = nu, type = type.reg, fitted = FALSE)
		pred <- predict(fit.svm, newdata = newX)
		fit <- list(object = fit.svm)
	}
	if(family$family == "binomial") {
		fit.svm <- svm(y = as.factor(Y), x = X, nu = nu, type = type.class, fitted = FALSE, probability = TRUE)
		pred <- attr(predict(fit.svm, newdata = newX, probability = TRUE), "prob")[, 2]
		fit <- list(object = fit.svm)
	}
	out <- list(pred = pred, fit = fit)
	class(out$fit) <- c("SL.svm")
	return(out)
}

predict.SL.svm <- function(object, newdata, family,...){
  .SL.require('e1071')
	if(family$family == "gaussian") {
		pred <- predict(object$object, newdata = newdata)
	}
	if(family$family == "binomial") {
		pred <- attr(predict(object$object, newdata = newdata, probability = TRUE), "prob")[, 2]
	}
	return(pred)
}
