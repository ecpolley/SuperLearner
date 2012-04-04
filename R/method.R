# outline for SuperLearner methods
# these should always have class 'SL.method'
# 
# The SuperLearner method is a coupling of the estimation algorithm for the algorithm weights (coefficients) and the model to combine the algorithms
# 
# 2 parts need to be included:
#   1) compute coefficients
#   2) compute predictions

method.template <- function() {
  out <- list(
  # require allows you to pass a character vector with required packages
  # use NULL if no required packages
  require = NULL,
  # computeCoef is a function that returns a list with two elements:
  # 1) coef: the weights (coefficients) for each algorithm 
  # 2) cvRisk: the V-fold CV risk for each algorithm
  computeCoef = function(Z, Y, libraryNames, obsWeights, control, verbose, ...) {
    cvRisk <- numeric()
    coef <- numeric()
    out <- list(cvRisk = cvRisk, coef = coef)
    return(out)
  },
  # computePred is a function that takes the weights and the predicted values from each algorithm in the library and combines them based on the model to output the super learner predicted values
  computePred = function(predY, coef, control, ...) {
    out <- crossprod(t(predY), coef)
    return(out)
  }
  )
  invisible(out)
}

write.method.template <- function(file = '', ...) {
  cat('method.template <- function() {\n  out <- list(\n    # require allows you to pass a character vector with required packages\n    # use NULL if no required packages\n    require = NULL,\n\n    # computeCoef is a function that returns a list with two elements:\n    # 1) coef: the weights (coefficients) for each algorithm\n    # 2) cvRisk: the V-fold CV risk for each algorithm\n    computeCoef = function(Z, Y, libraryNames, obsWeights, control, verbose, ...) {\n      cvRisk <- numeric()\n      coef <- numeric()\n      out <- list(cvRisk = cvRisk, coef = coef)\n      return(out)\n    },\n\n    # computePred is a function that takes the weights and the predicted values\n    # from each algorithm in the library and combines them based on the model to\n    # output the super learner predicted values\n    computePred = function(predY, coef, control, ...) {\n      out <- crossprod(t(predY), coef)\n      return(out)\n    }\n    )\n    invisible(out)\n  }', file = file, ...)
}

# examples:
method.NNLS <- function() {
  out <- list(
  require = 'nnls',
  computeCoef = function(Z, Y, libraryNames, verbose, obsWeights, ...) {
    # compute cvRisk
    cvRisk <- apply(Z, 2, function(x) mean(obsWeights * (x - Y)^2))
    names(cvRisk) <- libraryNames
    # compute coef
    fit.nnls <- nnls(sqrt(obsWeights) * Z, sqrt(obsWeights) * Y)
    if(verbose) {
			message(paste("Non-Negative least squares convergence: ", fit.nnls$mode==1))
		}
    initCoef <- coef(fit.nnls)
		initCoef[is.na(initCoef)] <- 0.0
    # normalize so sum(coef) = 1 if possible
		if(sum(initCoef) > 0) {
			coef <- initCoef/sum(initCoef)
		} else {
			warning("All algorithms have zero weight", call. = FALSE)
			coef <- initCoef
		}
    out <- list(cvRisk = cvRisk, coef = coef)
    return(out)
  },
  computePred = function(predY, coef, ...) {
    out <- crossprod(t(predY), coef)
    return(out)
  }
  )
  invisible(out)
}

method.NNLS2 <- function() {
  out <- list(
  require = 'quadprog',
  computeCoef = function(Z, Y, libraryNames, verbose, obsWeights, ...) {
    # compute cvRisk
    cvRisk <- apply(Z, 2, function(x) mean(obsWeights * (x - Y)^2))
    names(cvRisk) <- libraryNames
    # compute coef
    .NNLS <- function(x, y, wt) {
    	wX <- sqrt(wt) * x
    	wY <- sqrt(wt) * y
    	D <- t(wX) %*% wX
    	d <- t(t(wY) %*% wX)
    	A <- diag(ncol(wX))
    	b <- rep(0, ncol(wX))
    	fit <- solve.QP(Dmat = D, dvec = d, Amat = t(A), bvec = b, meq=0)
    	invisible(fit)
    }
    fit.nnls <- .NNLS(x = Z, y = Y, wt = obsWeights)
    initCoef <- fit.nnls$solution
    initCoef[initCoef < 0] <- 0.0
		initCoef[is.na(initCoef)] <- 0.0
    # normalize so sum(coef) = 1 if possible
		if(sum(initCoef) > 0) {
			coef <- initCoef/sum(initCoef)
		} else {
			warning("All algorithms have zero weight", call. = FALSE)
			coef <- initCoef
		}
    out <- list(cvRisk = cvRisk, coef = coef)
    return(out)
  },
  computePred = function(predY, coef, ...) {
    out <- crossprod(t(predY), coef)
    return(out)
  }
  )
  invisible(out)
}

method.NNloglik <- function() {
  out <- list(
  require = NULL,
  computeCoef = function(Z, Y, libraryNames, verbose, obsWeights, control, ...) {
    # compute cvRisk
    cvRisk <- apply(Z, 2, function(x) { -mean(obsWeights * ifelse(Y, log(x), log(1-x))) } )
    names(cvRisk) <- libraryNames
    # compute coef
    .NNloglik <- function(x, y, wt, start = rep(0, ncol(x))) {
    	# adapted from MASS pg 445
    	fmin <- function(beta, X, y, w) {
    		p <- plogis(crossprod(t(X), beta))
    		-sum(2 * w * ifelse(y, log(p), log(1-p)))
    	}
    	gmin <- function(beta, X, y, w) {
    		eta <- X %*% beta
    		p <- plogis(eta)
    		-2 * t(w * dlogis(eta) * ifelse(y, 1/p, -1/(1-p))) %*% X
    	}
    	fit <- optim(start, fmin, gmin, X = x, y = y, w = wt, method = "L-BFGS-B", lower = 0, ...)
    	invisible(fit)
    }
    tempZ <- trimLogit(Z, trim = control$trimLogit)
    fit.nnloglik <- .NNloglik(x = tempZ, y = Y, wt = obsWeights)
		if(verbose) {
			message(paste("Non-Negative log-likelihood convergence: ", fit.nnloglik$convergence == 0))
		}
		initCoef <- fit.nnloglik$par
    initCoef[initCoef < 0] <- 0.0
		initCoef[is.na(initCoef)] <- 0.0
    # normalize so sum(coef) = 1 if possible
		if(sum(initCoef) > 0) {
			coef <- initCoef/sum(initCoef)
		} else {
			warning("All algorithms have zero weight", call. = FALSE)
			coef <- initCoef
		}
    out <- list(cvRisk = cvRisk, coef = coef)
    return(out)
  },
  computePred = function(predY, coef, control, ...) {
    out <- plogis(crossprod(t(trimLogit(predY, trim = control$trimLogit)), coef))
    return(out)
  }
  )
  invisible(out)
}
