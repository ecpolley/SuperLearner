# knn{class}
# will only work with binomial
# to create additional algorithms with different values of k, for example k=20
# SL.knn20 <- function(..., k = 20) SL.knn(...,k = k)

SL.knn <- function(Y, X, newX, family, k = 10, ...) {
  .SL.require('class')
	if(family$family=="gaussian") {  
		stop("SL.knn only available for family = binomial()")
	}
	fit.knn <- class::knn(train = X, test = newX, k = k, cl = Y, prob = TRUE)
	pred <- (as.numeric(fit.knn) - 1) * attr(fit.knn, "prob") + (1 - (as.numeric(fit.knn) - 1)) * (1 - attr(fit.knn, "prob"))
	fit <- list(k = k)
	out <- list(pred = pred, fit=fit)
	class(out$fit) <- c("SL.knn")
	return(out)
}

# will need original Y and X data for this
predict.SL.knn <- function(object, newdata, X, Y, ...){
  .SL.require('class')
  fit.knn <- class::knn(train = X, test = newdata, k = object$k, cl = Y, prob = TRUE)
  pred <- (as.numeric(fit.knn) - 1) * attr(fit.knn, "prob") + (1 - (as.numeric(fit.knn) - 1)) * (1 - attr(fit.knn, "prob"))
  return(pred)
}