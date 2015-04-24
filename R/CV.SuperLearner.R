# V-fold Cross-validation wrapper for SuperLearner

CV.SuperLearner <- function(Y, X, V = 20, family = gaussian(), SL.library, method = 'method.NNLS', id = NULL, verbose = FALSE, control = list(saveFitLibrary = FALSE), cvControl = list(), obsWeights = NULL, saveAll = TRUE, parallel = "seq") {
  call <- match.call()
  N <- dim(X)[1L]
  
  # create CV folds:
  cvControl <- do.call('SuperLearner.CV.control', cvControl)
  insideV <- cvControl$V
  cvControl$V <- V
  folds <- CVFolds(N = N, id = id, Y = Y, cvControl = cvControl)
	cvControl$V <- insideV
  
  # check input:
  if(is.null(obsWeights)) {
		obsWeights <- rep(1, N)
	}
	if(!identical(length(obsWeights), N)) {
		stop("obsWeights vector must have the same dimension as Y")
	}
	
	# check method:
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
  
  # create placeholders:
  library <- .createLibrary(SL.library)
  libraryNames <- paste(library$library$predAlgorithm, library$screenAlgorithm[library$library$rowScreen], sep="_")
  k <- nrow(library$library)
  AllSL <- vector('list', V)
  names(AllSL) <- paste("training", 1:V, sep=" ")
	SL.predict <- rep(NA, N)
	discreteSL.predict <- rep.int(NA, N)
	whichDiscreteSL <- rep.int(NA, V)
	library.predict <- matrix(NA, nrow = N, ncol = k)
	colnames(library.predict) <- libraryNames
  
  # run SuperLearner:
  .crossValFun <- function(valid, Y, dataX, family, id, obsWeights, SL.library, method, verbose, control, cvControl, saveAll) {
    cvLearn <- dataX[-valid, , drop = FALSE]
    cvOutcome <- Y[-valid]
    cvValid <- dataX[valid, , drop = FALSE]
    cvId <- id[-valid]
    cvObsWeights <- obsWeights[-valid]
    
    fit.SL <- SuperLearner(Y = cvOutcome, X = cvLearn, newX = cvValid, family = family, SL.library = SL.library, method = method, id = cvId, verbose = verbose, control = control, cvControl = cvControl, obsWeights = cvObsWeights)
    
    out <- list(cvAllSL = if(saveAll) fit.SL, cvSL.predict = fit.SL$SL.predict, cvdiscreteSL.predict = fit.SL$library.predict[, which.min(fit.SL$cvRisk)], cvwhichDiscreteSL = names(which.min(fit.SL$cvRisk)), cvlibrary.predict = fit.SL$library.predict, cvcoef = fit.SL$coef)
    return(out)
  }
  ## Why is CV.SuperLearner not saving the output from SuperLearner, only the call name?
  ## if we add something like force() will this eval multiple times?
  
  if(parallel == "seq") {
    cvList <- lapply(folds, FUN = .crossValFun, Y = Y, dataX = X, family = family, SL.library = SL.library, method = method, id = id, obsWeights = obsWeights, verbose = verbose, control = control, cvControl = cvControl, saveAll = saveAll)
  } else if (parallel == 'multicore') {
    .SL.require('parallel')
    cvList <- parallel::mclapply(folds, FUN = .crossValFun, Y = Y, dataX = X, family = family, SL.library = SL.library, method = method, id = id, obsWeights = obsWeights, verbose = verbose, control = control, cvControl = cvControl, saveAll = saveAll, mc.set.seed = FALSE)
  } else if (inherits(parallel, 'cluster')) {
    cvList <- parallel::parLapply(parallel, x = folds, fun = .crossValFun, Y = Y, dataX = X, family = family, SL.library = SL.library, method = method, id = id, obsWeights = obsWeights, verbose = verbose, control = control, cvControl = cvControl, saveAll = saveAll)
  } else {
    stop('parallel option was not recognized, use parallel = "seq" for sequential computation.')
  }
  # check out Biobase::subListExtract to replace the lapply
  AllSL <- lapply(cvList, '[[', 'cvAllSL')
  SL.predict[unlist(folds, use.names = FALSE)] <- unlist(lapply(cvList, '[[', 'cvSL.predict'), use.names = FALSE)
  discreteSL.predict[unlist(folds, use.names = FALSE)] <- unlist(lapply(cvList, '[[', 'cvdiscreteSL.predict'), use.names = FALSE)
  whichDiscreteSL <- lapply(cvList, '[[', 'cvwhichDiscreteSL')
  library.predict[unlist(folds, use.names = FALSE), ] <- do.call('rbind', lapply(cvList, '[[', 'cvlibrary.predict'))
  coef <- do.call('rbind', lapply(cvList, '[[', 'cvcoef'))
  colnames(coef) <- libraryNames
  
  # put together output
  out <- list(call = call, AllSL = AllSL, SL.predict = SL.predict, discreteSL.predict = discreteSL.predict, whichDiscreteSL = whichDiscreteSL, library.predict = library.predict, coef = coef, folds = folds, V = V, libraryNames = libraryNames, SL.library = library, method = method, Y = Y)
  class(out) <- 'CV.SuperLearner'
  return(out)
}