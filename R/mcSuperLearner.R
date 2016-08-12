#  mcSuperLearner
#
#  Created by Eric Polley on 2011-01-01.
#
mcSuperLearner <- function(Y, X, newX = NULL, family = gaussian(), SL.library,
                           method = 'method.NNLS', id = NULL, verbose = FALSE,
                           control = list(), cvControl = list(), obsWeights = NULL,
                           env = parent.frame()) {

  # Begin timing how long SuperLearner takes to execute.
  time_start = proc.time()

  .SL.require('parallel')
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
  cvControl <- do.call('SuperLearner.CV.control', cvControl)

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
	if (is.character(family))
		family <- get(family, mode="function", envir=env)
	if (is.function(family))
		family <- family()
	if (is.null(family$family)) {
		print(family)
		stop("'family' not recognized")
	}

	if (family$family != "binomial" & isTRUE("cvAUC" %in% method$require)){
		stop("'method.AUC' is designed for the 'binomial' family only")
	}

  # create CV folds
	validRows <- CVFolds(N = N, id = id, Y = Y, cvControl = cvControl)

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

  # create function for the cross-validation step:
	.crossValFUN <- function(valid, Y, dataX, id, obsWeights, library, kScreen, k, p, libraryNames) {
	  tempLearn <- dataX[-valid, , drop = FALSE]
	  tempOutcome <- Y[-valid]
	  tempValid <- dataX[valid, , drop = FALSE]
	  tempWhichScreen <- matrix(NA, nrow = kScreen, ncol = p)
	  tempId <- id[-valid]
	  tempObsWeights <- obsWeights[-valid]

    # should this be converted to a lapply also?
		for(s in seq(kScreen)) {
		  screen_fn = get(library$screenAlgorithm[s], envir = env)
			testScreen <- try(do.call(screen_fn, list(Y = tempOutcome, X = tempLearn, family = family, id = tempId, obsWeights = tempObsWeights)))
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

    # should this be converted to a lapply also?
    out <- matrix(NA, nrow = nrow(tempValid), ncol = k)
		for(s in seq(k)) {
		  pred_fn = get(library$library$predAlgorithm[s], envir = env)
			testAlg <- try(do.call(pred_fn, list(Y = tempOutcome, X = subset(tempLearn, select = tempWhichScreen[library$library$rowScreen[s], ], drop=FALSE), newX = subset(tempValid, select = tempWhichScreen[library$library$rowScreen[s], ], drop=FALSE), family = family, id = tempId, obsWeights = tempObsWeights)))
			if(inherits(testAlg, "try-error")) {
				warning(paste("Error in algorithm", library$library$predAlgorithm[s], "\n  The Algorithm will be removed from the Super Learner (i.e. given weight 0) \n" ))
        # errorsInCVLibrary[s] <<- 1
        # '<<-' doesn't work with mclapply.
			} else {
				out[, s] <- testAlg$pred
			}
			# verbose will not work in the GUI, but works in the terminal
			if(verbose) message(paste("CV", libraryNames[s]))
		} #end library
	  invisible(out)
	}
  # the lapply performs the cross-validation steps to create Z
  # additional steps to put things in the correct order
  # rbind unlists the output from lapply
  # need to unlist folds to put the rows back in the correct order
	time_train = system.time({
	  Z[unlist(validRows, use.names = FALSE), ] <- do.call('rbind', parallel::mclapply(validRows, FUN = .crossValFUN, Y = Y, dataX = X, id = id, obsWeights = obsWeights, library = library, kScreen = kScreen, k = k, p = p, libraryNames = libraryNames))

    # Check for errors. If any algorithms had errors, replace entire column with
    # 0 even if error is only in one fold.
    errorsInCVLibrary <- apply(Z, 2, function(x) any(is.na(x)))
    if (sum(errorsInCVLibrary) > 0) {
		  Z[, as.logical(errorsInCVLibrary)] <- 0
	  }
	  if (all(Z == 0)) {
		  stop("All algorithms dropped from library")
	  }

    # Compute weights for each algorithm in library.
    getCoef <- method$computeCoef(Z = Z, Y = Y, libraryNames = libraryNames, obsWeights = obsWeights, control = control, verbose = verbose)
    coef <- getCoef$coef
    names(coef) <- libraryNames

  }) # Finish timing.

  # Set a default in case the method does not return the optimizer result.
  if(!("optimizer" %in% names(getCoef))) {
    getCoef["optimizer"] <- NA
  }

  # now fit all algorithms in library on entire learning data set and predict on newX
  m <- dim(newX)[1L]
  predY <- matrix(NA, nrow = m, ncol = k)
  # whichScreen <- matrix(NA, nrow = kScreen, ncol = p)

  .screenFun <- function(fun, list) {
    screen_fn = get(fun, envir = env)
    testScreen <- try(do.call(screen_fn, list))
    if(inherits(testScreen, "try-error")) {
  		warning(paste("replacing failed screening algorithm,", fun, ", with All() in full data", "\n "))
  		out <- rep(TRUE, ncol(list$X))
  	} else {
  		out <- testScreen
  	}
    return(out)
  }

  time_predict = system.time({
    whichScreen <- t(sapply(library$screenAlgorithm, FUN = .screenFun, list = list(Y = Y, X = X, family = family, id = id, obsWeights = obsWeights)))

    # change to sapply?
    # for(s in 1:k) {
    #   testAlg <- try(do.call(library$library$predAlgorithm[s], list(Y = Y, X = subset(X, select = whichScreen[library$library$rowScreen[s], ], drop=FALSE), newX = subset(newX, select = whichScreen[library$library$rowScreen[s], ], drop=FALSE), family = family, id = id, obsWeights = obsWeights)))
    #   if(inherits(testAlg, "try-error")) {
    #     warning(paste("Error in algorithm", library$library$predAlgorithm[s], " on full data", "\n  The Algorithm will be removed from the Super Learner (i.e. given weight 0) \n" ))
    #     errorsInLibrary[s] <- 1
    #   } else {
    #     predY[, s] <- testAlg$pred
    #   }
    #   if(control$saveFitLibrary) {
    #     fitLibrary[[s]] <- testAlg$fit
    #   }
    #   if(verbose) {
    #     message(paste("full", libraryNames[s]))
    #   }
    # }

    # assign in envirnoments doesn't work with mc and snow, change .predFun to return a list with both pred and fitLibrary elements and then parse the two.
    .predFun <- function(index, lib, Y, dataX, newX, whichScreen, family, id, obsWeights, verbose, control, libraryNames) {
      out <- list(pred = NA, fitLibrary = NULL)
      pred_fn = get(lib$predAlgorithm[index], envir = env)
      testAlg <- try(do.call(pred_fn, list(Y = Y, X = subset(dataX, select = whichScreen[lib$rowScreen[index], ], drop=FALSE), newX = subset(newX, select = whichScreen[lib$rowScreen[index], ], drop=FALSE), family = family, id = id, obsWeights = obsWeights)))
      if(inherits(testAlg, "try-error")) {
        warning(paste("Error in algorithm", lib$predAlgorithm[index], " on full data", "\n  The Algorithm will be removed from the Super Learner (i.e. given weight 0) \n" ))
        out$pred <- rep.int(NA, times = nrow(newX))
      } else {
        out$pred <- testAlg$pred
        if(control$saveFitLibrary) {
          # eval(bquote(fitLibrary[[.(index)]] <- .(testAlg$fit)), envir = fitLibEnv)
          out$fitLibrary <- testAlg$fit
        }
      }
      if(verbose) {
        message(paste("full", libraryNames[index]))
      }
      invisible(out)
    }


    foo <- parallel::mclapply(seq(k), FUN = .predFun, lib = library$library, Y = Y, dataX = X, newX = newX, whichScreen = whichScreen, family = family, id = id, obsWeights = obsWeights, verbose = verbose, control = control, libraryNames = libraryNames)
    predY <- do.call('cbind', lapply(foo, '[[', 'pred'))
    assign('fitLibrary', lapply(foo, '[[', 'fitLibrary'), envir = fitLibEnv)
    rm(foo)
    # predY <- do.call('cbind', mclapply(seq(k), FUN = .predFun, lib = library$library, Y = Y, dataX = X, newX = newX, whichScreen = whichScreen, family = family, id = id, obsWeights = obsWeights, verbose = verbose, control = control, libraryNames = libraryNames))

    # Check for errors.
    errorsInLibrary <- apply(predY, 2, function(xx) any(is.na(xx)))
    if (sum(errorsInLibrary) > 0) {
      if (sum(coef[as.logical(errorsInLibrary)]) > 0) {
        warning(paste("re-running estimation of coefficients removing failed algorithm(s) \n Orignial coefficients are: \n", coef, "\n"))
        Z[, as.logical(errorsInLibrary)] <- 0
        if (all(Z == 0)) {
          stop("All algorithms dropped from library")
        }
        getCoef <- method$computeCoef(Z = Z, Y = Y, libraryNames = libraryNames, obsWeights = obsWeights, control = control, verbose = verbose)
        coef <- getCoef$coef
        names(coef) <- libraryNames
      } else {
        warning("coefficients already 0 for all failed algorithm(s)")
      }
    }

    # Compute super learner predictions on newX.
    getPred <- method$computePred(predY = predY, coef = coef, control = control)

  }) # Finish timing.

	# Add names of algorithms to the predictions.
	colnames(predY) <- libraryNames

	# Clean up when errors in library.
	if (sum(errorsInCVLibrary) > 0) {
		getCoef$cvRisk[as.logical(errorsInCVLibrary)] <- NA
	}

	# Finish timing the full SuperLearner execution.
	time_end = proc.time()

	# Compile execution times.
	times = list(everything = time_end - time_start,
	             train = time_train,
	             predict = time_predict)

  # Put everything together in a list.
	out <- list(
	    call = call,
	    libraryNames = libraryNames,
	    SL.library = library,
	    SL.predict = getPred,
	    coef = coef,
	    library.predict = predY,
	    Z = Z,
	    cvRisk = getCoef$cvRisk,
	    family = family,
	    fitLibrary = get('fitLibrary', envir = fitLibEnv),
	    id = id,
	    varNames = varNames,
	    validRows = validRows,
	    method = method,
	    whichScreen = whichScreen,
	    control = control,
	    errorsInCVLibrary = errorsInCVLibrary,
	    errorsInLibrary = errorsInLibrary,
	    obsWeights = obsWeights,
	    metaOptimizer = getCoef$optimizer,
	    env = env,
	    times = times
	  )
	class(out) <- c("SuperLearner")
	return(out)
}
