
SL.step <- function(Y, X, newX, family, direction = "both", trace = 0, k = 2, ...) {
	fit.glm <- glm(Y ~ ., data = X, family = family)
	fit.step <- step(fit.glm, direction = direction, trace = trace, k = k)
	pred <- predict(fit.step, newdata = newX, type = "response")
	fit <- list(object = fit.step)
	out <- list(pred = pred, fit=fit)
	class(out$fit) <- c("SL.step")
	return(out)
}

SL.step.forward <- function(Y, X, newX, family, direction = "forward", trace = 0, k = 2, ...) {
	fit.glm <- glm(Y ~ ., data = X, family = family)
	fit.step <- step(glm(Y ~ 1, data = X, family = family), scope = formula(fit.glm), direction = direction, trace = trace, k = k)
	pred <- predict(fit.step, newdata = newX, type = "response")
	fit <- list(object = fit.step)
	out <- list(pred = pred, fit = fit)
	class(out$fit) <- c("SL.step")
	return(out)
}

SL.step.interaction <- function(Y, X, newX, family, direction = "both", trace = 0, k = 2, ...) {
	fit.glm <- glm(Y ~ ., data = X, family = family)
	fit.step <- step(fit.glm, scope = Y ~ .^2, direction = direction, trace = trace, k = k)
	pred <- predict(fit.step, newdata = newX, type = "response")
	fit <- list(object = fit.step)
	out <- list(pred = pred, fit=fit)
	class(out$fit) <- c("SL.step")
	return(out)
}

# 
predict.SL.step <- function(object, newdata,...) {
	predict(object = object$object, newdata = newdata, type = "response")
}

SL.stepAIC <- function(Y, X, newX, family, direction = "both", steps = 30, k = log(nrow(X)), ...) {
  .SL.require('MASS')
	g0 <- glm(Y ~ 1, data = X, family = family)
	upper <- formula(paste("~", paste(colnames(X), collapse="+")))
	lower <- formula("~1")
	fit.step <- MASS::stepAIC(g0, scope = list(upper = upper, lower = lower), direction = direction, k = k, trace = 0, steps = steps)
	pred <- predict(fit.step, newdata = newX, type = "response")
	fit <- list(object = fit.step)
	out <- list(pred = pred, fit=fit)
	class(out$fit) <- c("SL.stepAIC")
	return(out)
}

predict.SL.stepAIC <- function(object, newdata, ...) {
  .SL.require('MASS')
	pred <- predict(object = object$object, newdata = newdata, type = "response")
	return(pred)
}