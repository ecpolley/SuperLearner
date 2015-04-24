# gbm{gbm}
# generalized boosting algorithm
# can alter number ot trees in intial fit
# also alter interaction depth (SL.gbm.1 and SL.gbm.2)
SL.gbm <- function(Y, X, newX, family, obsWeights, gbm.trees = 10000, interaction.depth = 2, ...) {
  .SL.require('gbm')
  gbm.model <- as.formula(paste("Y~", paste(colnames(X), collapse="+")))
	if(family$family == "gaussian") {  
		fit.gbm <- gbm::gbm(formula = gbm.model, data = X, distribution = "gaussian", n.trees = gbm.trees, interaction.depth = interaction.depth, cv.folds = 5, keep.data = TRUE, weights = obsWeights, verbose = FALSE)
	}
	if(family$family == "binomial") {
		fit.gbm <- gbm::gbm(formula = gbm.model, data = X, distribution = "bernoulli", n.trees = gbm.trees, interaction.depth = interaction.depth, cv.folds = 5, keep.data = TRUE, verbose = FALSE, weights = obsWeights)
	}
	best.iter <- gbm::gbm.perf(fit.gbm, method = "cv", plot.it = FALSE)
	pred <- predict(fit.gbm, newdata = newX, best.iter, type = "response")
	fit <- list(object = fit.gbm, n.trees = best.iter)
	out <- list(pred = pred, fit = fit)
	class(out$fit) <- c("SL.gbm")
	return(out)
}

predict.SL.gbm <- function(object, newdata,...){
  .SL.require('gbm')
	pred <- predict(object$object, newdata = newdata, n.trees = object$n.trees, type = "response")
	return(pred)
}

