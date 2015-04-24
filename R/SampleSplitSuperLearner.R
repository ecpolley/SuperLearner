#  SuperLearner for Sample Split instead of V-fold CV
#  
#  Created by Eric Polley on 2014-04-15.
# 
SampleSplitSuperLearner <- function(Y, X, newX = NULL, family = gaussian(), SL.library, method = 'method.NNLS', id = NULL, verbose = FALSE, control = list(), split = 0.8, obsWeights = NULL) {
  if(is.character(method)) {
    if(exists(method, mode = 'list')) {
      method <- get(method, mode = 'list')
    } else if(exists(method, mode = 'function')) {
      method <- get(method, mode = 'function')()
    }
  } else if(is.function(method)) {
    method <- method()
  }
  if(!is.list(method)) {
    stop("method is not in the appropriate format. Check out help('method.template')")
  }
  if(!is.null(method$require)) {
	  sapply(method$require, function(x) require(force(x), character.only = TRUE))
	}
  # get defaults for controls and make sure in correct format
  control <- do.call('SuperLearner.control', control)
  
  # put together the library
  # should this be in a new environment?
  library <- .createLibrary(SL.library)
	.check.SL.library(library = c(unique(library$library$predAlgorithm), library$screenAlgorithm))
	
	call <- match.call(expand.dots = TRUE)
  # should we be checking X and newX for data.frame?
  # data.frame not required, but most of the built-in wrappers assume a data.frame
  if(!inherits(X, 'data.frame')) message('X is not a data frame. Check the algorithms in SL.library to make sure they are compatible with non data.frame inputs')
  varNames <- colnames(X)
  N <- dim(X)[1L]
  p <- dim(X)[2L]
  k <- nrow(library$library)
  kScreen <- length(library$screenAlgorithm)
  Z <- matrix(NA, N, k)
  libraryNames <- paste(library$library$predAlgorithm, library$screenAlgorithm[library$library$rowScreen], sep="_")
	
	# split data
	# todo: allow user to supply these, in a cvControl like argument
	# split should be either a single value between 0 and 1, OR a vector with the validRows
	if(length(split) == 1) {
		if(split <= 0 | split >= 1) stop("invalid split value, must be between 0 and 1")
		validRows <- sample.int(N, size = round((1 - split)*N))
		trainRows <- setdiff(seq(N), validRows)
	} else {
		if(length(split) >= N) stop("split should be a vector with the row numbers for the samples in the validation split")
		validRows <- split
		trainRows <- setdiff(seq(N), validRows)
	}
	
	# put fitLibrary in it's own environment to locate later
	fitLibEnv <- new.env()
	assign('fitLibrary', vector('list', length = k), envir = fitLibEnv)
	assign('libraryNames', libraryNames, envir = fitLibEnv)
	evalq(names(fitLibrary) <- libraryNames, envir = fitLibEnv)
	
  # errors* records if an algorithm stops either in the CV step and/or in full data
	errorsInCVLibrary <- rep(0, k)
	errorsInLibrary <- rep(0, k)
	
  # if newX is missing, use X
	if(is.null(newX)) {
		newX <- X
	}
  # Are these checks still required?
	if(!identical(colnames(X), colnames(newX))) {
		stop("The variable names and order in newX must be identical to the variable names and order in X")
	}
	if (sum(is.na(X)) > 0 | sum(is.na(newX)) > 0 | sum(is.na(Y)) > 0) {
		stop("missing data is currently not supported. Check Y, X, and newX for missing values")
	}
	if (!is.numeric(Y)) {
		stop("the outcome Y must be a numeric vector")
	}
  # family can be either character or function, so these lines put everything together (code from glm())
	if(is.character(family))
		family <- get(family, mode="function", envir=parent.frame())
	if(is.function(family))
		family <- family()
	if (is.null(family$family)) {
		print(family)
		stop("'family' not recognized")
	}
	
	if (family$family != "binomial" & isTRUE("cvAUC" %in% method$require)){
		stop("'method.AUC' is designed for the 'binomial' family only")
	}
	
  # test id
	if(is.null(id)) {
		id <- seq(N)
	}
	if(!identical(length(id), N)) {
		stop("id vector must have the same dimension as Y")
	}
  # test observation weights
	if(is.null(obsWeights)) {
		obsWeights <- rep(1, N)
	}
	if(!identical(length(obsWeights), N)) {
		stop("obsWeights vector must have the same dimension as Y")
	}
	
  # create datasets for the prediction algorithms
	# Do we need to make a copy of the data here?
	tempLearn <- X[trainRows, , drop = FALSE]
	tempOutcome <- Y[trainRows]
	tempValid <- X[validRows, , drop = FALSE]
	tempValidOutcome <- Y[validRows]
	tempWhichScreen <- matrix(NA, nrow = kScreen, ncol = p)
	tempId <- id[trainRows]
	tempObsWeights <- obsWeights[trainRows]
	  
    # should this be converted to a lapply also?
	for(s in seq(kScreen)) {
		testScreen <- try(do.call(library$screenAlgorithm[s], list(Y = tempOutcome, X = tempLearn, family = family, id = tempId, obsWeights = tempObsWeights)))
		if(inherits(testScreen, "try-error")) {
			warning(paste("replacing failed screening algorithm,", library$screenAlgorithm[s], ", with All()", "\n ")) 
			tempWhichScreen[s, ] <- TRUE
		} else {
			tempWhichScreen[s, ] <- testScreen
		}
		if(verbose) {
			message(paste("Number of covariates in ", library$screenAlgorithm[s], " is: ", sum(tempWhichScreen[s, ]), sep = ""))
		}
	} #end screen
		
    
    Z <- matrix(NA, nrow = nrow(tempValid), ncol = k) # k is the number of algorithms in the library
		# should we replace the subset() call below?
		for(s in seq(k)) {
		  testAlg <- try({
		    select <- tempWhichScreen[library$library$rowScreen[s], ]
		    if (all(select)) {
		      tempX <- tempLearn
		      tempnewX <- tempValid
		    } else {
		      tempX <- subset(tempLearn, select = which(select), drop=FALSE)
		      tempnewX <- subset(tempValid, select = which(select), drop=FALSE)
		    }
		    do.call(library$library$predAlgorithm[s], list(
		      Y = tempOutcome,
		      X = tempX,
		      newX = tempnewX,
		      family = family, id = tempId, obsWeights = tempObsWeights))
		  })
			if(inherits(testAlg, "try-error")) {
				warning(paste("Error in algorithm", library$library$predAlgorithm[s], "\n  The Algorithm will be removed from the Super Learner (i.e. given weight 0) \n" )) 
        # errorsInCVLibrary[s] <<- 1
			} else {
				Z[, s] <- testAlg$pred
			}
			if(verbose) message(paste("CV", libraryNames[s]))
		} #end library

	# row order in Z should match row order in tempValid and tempValidOutcome
	# Z[unlist(validRows, use.names = FALSE), ] <- do.call('rbind', lapply(validRows, FUN = .crossValFUN, Y = Y, dataX = X, id = id, obsWeights = obsWeights, library = library, kScreen = kScreen, k = k, p = p, libraryNames = libraryNames))
	
# check for errors. If any algorithms had errors, replace entire column with 0 even if error is only in one fold.

  errorsInCVLibrary <- apply(Z, 2, function(x) any(is.na(x)))
  if(sum(errorsInCVLibrary) > 0) {
		Z[, as.logical(errorsInCVLibrary)] <- 0 
	}
	if(all(Z == 0)) {
		stop("All algorithms dropped from library")
	}
	
  # compute weights for each algorithm in library:
  getCoef <- method$computeCoef(Z = Z, Y = tempValidOutcome, libraryNames = libraryNames, obsWeights = obsWeights[validRows], control = control, verbose = verbose)
  coef <- getCoef$coef
  names(coef) <- libraryNames

  # now fit all algorithms in library on entire learning data set and predict on newX
  m <- dim(newX)[1L]
  predY <- matrix(NA, nrow = m, ncol = k)
  # whichScreen <- matrix(NA, nrow = kScreen, ncol = p)
  
  .screenFun <- function(fun, list) {
    testScreen <- try(do.call(fun, list))
    if(inherits(testScreen, "try-error")) {
  		warning(paste("replacing failed screening algorithm,", fun, ", with All() in full data", "\n ")) 
  		out <- rep(TRUE, ncol(list$X))
  	} else {
  		out <- testScreen
  	}
    return(out)
  }
  whichScreen <- t(sapply(library$screenAlgorithm, FUN = .screenFun, list = list(Y = Y, X = X, family = family, id = id, obsWeights = obsWeights)))
	
  .predFun <- function(index, lib, Y, dataX, newX, whichScreen, family, id, obsWeights, verbose, control, libraryNames) {
    testAlg <- try({
      select <- whichScreen[lib$rowScreen[index], ]
      if (all(select)) {
        tempX <- dataX
        tempnewX <- newX
      } else {
        tempX <- subset(dataX, select = which(select), drop=FALSE)
        tempnewX <- subset(newX, select = which(select), drop=FALSE)
      }
      do.call(lib$predAlgorithm[index], list(
        Y = Y,
        X = tempX,
        newX = tempnewX,
        family = family, id = id, obsWeights = obsWeights))
    })
    # testAlg <- try(do.call(lib$predAlgorithm[index], list(Y = Y, X = dataX[, whichScreen[lib$rowScreen[index], drop = FALSE]], newX = newX[, whichScreen[lib$rowScreen[index], drop = FALSE]], family = family, id = id, obsWeights = obsWeights)))
    if(inherits(testAlg, "try-error")) {
      warning(paste("Error in algorithm", lib$predAlgorithm[index], " on full data", "\n  The Algorithm will be removed from the Super Learner (i.e. given weight 0) \n" )) 
      out <- rep.int(NA, times = nrow(newX))
    } else {
      out <- testAlg$pred
      if(control$saveFitLibrary) {
        eval(bquote(fitLibrary[[.(index)]] <- .(testAlg$fit)), envir = fitLibEnv)
      }
    }
    if(verbose) {
      message(paste("full", libraryNames[index]))
    }
    invisible(out)
  }
  predY <- do.call('cbind', lapply(seq(k), FUN = .predFun, lib = library$library, Y = Y, dataX = X, newX = newX, whichScreen = whichScreen, family = family, id = id, obsWeights = obsWeights, verbose = verbose, control = control, libraryNames = libraryNames))
  
  # check for errors
	errorsInLibrary <- apply(predY, 2, function(xx) any(is.na(xx)))
	if(sum(errorsInLibrary) > 0) {
		if(sum(coef[as.logical(errorsInLibrary)]) > 0) {
			warning(paste("re-running estimation of coefficients removing failed algorithm(s) \n Orignial coefficients are: \n", coef, "\n"))
			Z[, as.logical(errorsInLibrary)] <- 0
			if(all(Z == 0)) {
				stop("All algorithms dropped from library")
			}
      getCoef <- method$computeCoef(Z = Z, Y = tempValidOutcome, libraryNames = libraryNames, obsWeights = obsWeights[validRows], control = control, verbose = verbose)
      coef <- getCoef$coef
      names(coef) <- libraryNames
		} else {
			warning("coefficients already 0 for all failed algorithm(s)")
		}
	}
	
  # compute super learner predictions on newX
	getPred <- method$computePred(predY = predY, coef = coef, control = control)
	
	# add names of algorithms to the predictions
	colnames(predY) <- libraryNames
	# clean up when errors in library
	if(sum(errorsInCVLibrary) > 0) {
		getCoef$cvRisk[as.logical(errorsInCVLibrary)] <- NA
	}
	
  # put everything together in a list
  out <- list(call = call, libraryNames = libraryNames, SL.library = library, SL.predict = getPred, coef = coef, library.predict = predY, Z = Z, cvRisk = getCoef$cvRisk, family = family, fitLibrary = get('fitLibrary', envir = fitLibEnv), varNames = varNames, validRows = validRows, method = method, whichScreen = whichScreen, control = control, split = split, errorsInCVLibrary = errorsInCVLibrary, errorsInLibrary = errorsInLibrary)
	class(out) <- c("SuperLearner")
	return(out)
}
