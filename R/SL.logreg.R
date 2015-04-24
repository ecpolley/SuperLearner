# logreg {LogicReg}

# currently uses CV to select model size
SL.logreg <- function(Y, X, newX, family, ntrees = c(1, 3), nleaves = c(1, 7), kfold = 10, ...) {
  .SL.require('LogicReg')
	if(family$family == "gaussian") {
		fit.cv.logreg <- LogicReg::logreg(resp = Y, bin = X, type = 2, select = 3, ntrees = ntrees, nleaves = nleaves, kfold = kfold)
		uu <- fit.cv.logreg$cvscores
		uu1 <- uu[, 1] * 1000 + uu[, 2]
		uu2 <- unique(uu1)
		vv <- uu[uu[, 3] == uu[, 4], c(1, 2, 6, 5, 8, 7)]
		for (i in 1:length(uu2)) {
		    vv[i, 4] <- sqrt(var(uu[uu1 == uu2[i], 5]))
		    vv[i, 6] <- sqrt(var(uu[uu1 == uu2[i], 7]))
		}
		names(vv)[4:6] <- c("train.sd", "cv/test", "cv/test.sd")
		min.ntree <- vv[which.min(vv[,5]), 1]
		min.nleaves <- vv[which.min(vv[,5]), 2]
		fit.logreg <- LogicReg::logreg(resp = Y, bin = X, type = 2, select = 1, ntrees = min.ntree, nleaves = min.nleaves)
	}
	if(family$family == "binomial") {
		fit.cv.logreg <- LogicReg::logreg(resp = Y, bin = X, type = 3, select = 3, ntrees = ntrees, nleaves = nleaves, kfold = kfold)
		uu <- fit.cv.logreg$cvscores
		uu1 <- uu[, 1] * 1000 + uu[, 2]
		uu2 <- unique(uu1)
		vv <- uu[uu[, 3] == uu[, 4], c(1, 2, 6, 5, 8, 7)]
		for (i in 1:length(uu2)) {
		    vv[i, 4] <- sqrt(var(uu[uu1 == uu2[i], 5]))
		    vv[i, 6] <- sqrt(var(uu[uu1 == uu2[i], 7]))
		}
		names(vv)[4:6] <- c("train.sd", "cv/test", "cv/test.sd")
		min.ntree <- vv[which.min(vv[, 5]), 1]
		min.nleaves <- vv[which.min(vv[, 5]), 2]
		fit.logreg <- LogicReg::logreg(resp = Y, bin = X, type = 3, select = 1, ntrees = min.ntree, nleaves = min.nleaves)
	}
	pred <- predict(fit.logreg, newbin = newX)
	fit <- list(object = fit.logreg)
	out <- list(pred = pred, fit = fit)
	class(out$fit) <- c("SL.logreg")
	return(out)
}

# 
predict.SL.logreg <- function(object, newdata, family, X=NULL, Y=NULL,...) {
  .SL.require('LogicReg')
	pred <- predict(object$object, newbin=newdata)
	return(pred)
}

