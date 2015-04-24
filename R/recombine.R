# These functions take an existing SuperLearner or CV.SuperLearner fit, 
# re-fits the ensemble metalearning step using a new metalearning method,
# specified in the `method` argument and returns the new fit.  
# This saves a lot of computation time since we don't have to re-compute the Z 
# matrix of cv predicted values by cross-validating each base learner a second time.
# The recombineSL and recombineCVSL functions are stripped down versions of the 
# original SuperLearner and CV.SuperLearner functions by Eric C. Polley.


recombineSL <- function(object, Y, method = "method.NNloglik", verbose = FALSE) {
  
  if (!inherits(object, "SuperLearner")) {
    stop("The supplied 'object' is not of class, SuperLearner.")
  }
  
  if (is.character(method)) {
    if (exists(method, mode = 'list')) {
      method <- get(method, mode = 'list')
    } else if (exists(method, mode = 'function')) {
      method <- get(method, mode = 'function')()
    }
  } else if (is.function(method)) {
    method <- method()
  }
  if (!is.list(method)) {
    stop("method is not in the appropriate format. Check out help('method.template')")
  }
  if (!is.null(method$require)) {
    sapply(method$require, function(x) require(force(x), character.only = TRUE))
  }
  if (identical(object$method, method, ignore.environment=TRUE)) {
    # May want to modify this "if" statement because method.AUC with different args look the same here
    warning("The new method supplied is identical to the existing method.")
  }
  
  # get relevant objects from the SuperLearner fit object
  call <- object$call
  obsWeights <- object$obsWeights
  control <- object$control
  cvControl <- object$cvControl
  family <- object$family
  library <- object$SL.library
  libraryNames <- object$libraryNames
  fitLibrary <- object$fitLibrary
  varNames <- object$varNames
  validRows <- object$validRows
  Z <- object$Z
  predY <- object$library.predict
  whichScreen <- object$whichScreen
  errorsInCVLibrary <- object$errorsInCVLibrary
  errorsInLibrary <- object$errorsInLibrary 
  
  # put fitLibrary in it's own environment to locate later
  fitLibEnv <- new.env()
  assign('fitLibrary', fitLibrary, envir = fitLibEnv)
  N <- dim(Z)[1L]
  k <- nrow(library$library)
  if (N != length(Y)) {
    stop("length(Y) does not match nrow(Z). Verify that Y is the same outcome variable that 'object' was trained on.")
  }
  if (is.null(obsWeights)) {
    obsWeights <- rep(1, N)
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
  if (family$family != "binomial" & isTRUE("cvAUC" %in% method$require)) {
    stop("'method.AUC' is designed for the 'binomial' family only")
  }
  
  # Re-compute weights for each algorithm in library using the new metalearner method:
  getCoef <- method$computeCoef(Z = Z, Y = Y, libraryNames = libraryNames, 
                                obsWeights = obsWeights, control = control, 
                                verbose = verbose)
  coef <- getCoef$coef
  names(coef) <- libraryNames
  
  # compute super learner predictions on newX
  getPred <- method$computePred(predY = predY, coef = coef, control = control)
  
  # clean up when errors in library
  if(sum(errorsInCVLibrary) > 0) {
    getCoef$cvRisk[as.logical(errorsInCVLibrary)] <- NA
  }
  
  # put everything together in a list
  out <- list(call = call, 
              libraryNames = libraryNames, 
              SL.library = library, 
              SL.predict = getPred, 
              coef = coef, 
              library.predict = predY, 
              Z = Z, 
              cvRisk = getCoef$cvRisk, 
              family = family, 
              fitLibrary = get('fitLibrary', envir = fitLibEnv), 
              varNames = varNames, 
              validRows = validRows, 
              method = method, 
              whichScreen = whichScreen, 
              control = control,  
              cvControl = cvControl, 
              errorsInCVLibrary = errorsInCVLibrary, 
              errorsInLibrary = errorsInLibrary)
  class(out) <- c("SuperLearner")
  return(out)
}



# This function takes an existing CV.SuperLearner object and for each of the V
# cross-validation folds, it re-fits the ensemble using a new metalearning method,
# specified by the `method` argument, and returns a new CV.SuperLearner object.  
# This saves a lot of computation time since, for all V iterations, we can skip re-computing 
# the Z matrix of cv predicted values by cross-validating each base learner a second time. 
# The recombineCVSL function is a re-worked version of the original CV.SuperLearner function by Eric C. Polley.

recombineCVSL <- function(object, method = "method.NNloglik", verbose = FALSE, saveAll = TRUE, parallel = "seq") {
  
  if (!inherits(object, "CV.SuperLearner")) {
    stop("The supplied 'object' is not of class, CV.SuperLearner.")
  }
  if (is.character(method)) {
    if (exists(method, mode = 'list')) {
      method <- get(method, mode = 'list')
    } else if (exists(method, mode = 'function')) {
      method <- get(method, mode = 'function')()
    }
  } else if (is.function(method)) {
    method <- method()
  }
  if (!is.list(method)) {
    stop("method is not in the appropriate format. Check out help('method.template')")
  }
  if (!is.null(method$require)) {
    sapply(method$require, function(x) require(force(x), character.only = TRUE))
  }
  if (identical(object$method, method, ignore.environment=TRUE)) {
    # May want to modify this "if" statement because method.AUC with different args look the same here
    warning("The new method supplied is identical to the existing method.")
  }
  
  # get relevant objects from the CV.SuperLearner object
  call <- object$call
  library <- object$SL.library
  libraryNames <- object$libraryNames
  folds <- object$folds
  V <- object$V
  oldAllSL <- object$AllSL
  Y <- object$Y
  N <- length(object$Y)
  if (N != length(Y)) {
    stop("length(Y) does not match nrow(Z). Verify that Y is the same outcome variable that 'fit' was trained on.")
  }
  
  k <- nrow(library$library)
  AllSL <- vector('list', V)  #Need to relearn SL fits using recombineSL and update this list
  names(AllSL) <- paste("training", 1:V, sep=" ")
  SL.predict <- rep(NA, N)
  discreteSL.predict <- object$discreteSL.predict  #Maybe we should use the versions created below, and keep the NA versions here
  whichDiscreteSL <- object$whichDiscreteSL
  library.predict <- object$library.predict
  
  # Get required arguments
  family <- object$AllSL[[1]]$family
  vlist <- as.list(seq(V))
  names(vlist) <- names(folds)
  # Re-fit SuperLearner:
  .crossValFun <- function(v, folds, oldAllSL, Y, method, verbose, saveAll) {
    # Modified from equivalent internal SuperLearner function
    fit <- oldAllSL[[v]]
    valid <- folds[[v]]
    cvOutcome <- Y[-valid]
    fit.SL <- recombineSL(object = fit, Y = cvOutcome, method = method, verbose = verbose)
    out <- list(cvAllSL = if(saveAll) fit.SL, cvSL.predict = fit.SL$SL.predict, cvdiscreteSL.predict = fit.SL$library.predict[, which.min(fit.SL$cvRisk)], cvwhichDiscreteSL = names(which.min(fit.SL$cvRisk)), cvlibrary.predict = fit.SL$library.predict, cvcoef = fit.SL$coef)
    return(out)
  }
  
  if(parallel == "seq") {
    cvList <- lapply(vlist, FUN = .crossValFun, folds = folds, oldAllSL = oldAllSL, Y = Y, method = method, verbose = verbose, saveAll = saveAll)
  } else if (parallel == 'multicore') {
    # not tested
    .SL.require('parallel')
    cvList <- parallel::mclapply(vlist, FUN = .crossValFun, folds = folds, oldAllSL = oldAllSL, Y = Y, method = method, verbose = verbose, saveAll = saveAll)
  } else if (inherits(parallel, 'cluster')) {
    # not tested
    cvList <- parallel::parLapply(parallel, x = vlist, fun = .crossValFun, folds = folds, oldAllSL = oldAllSL, Y = Y, method = method, verbose = verbose, saveAll = saveAll)
  } else {
    stop('parallel option was not recognized, use parallel = "seq" for sequential computation.')
  }

  AllSL <- lapply(cvList, '[[', 'cvAllSL')
  SL.predict[unlist(folds, use.names = FALSE)] <- unlist(lapply(cvList, '[[', 'cvSL.predict'), use.names = FALSE)
  discreteSL.predict[unlist(folds, use.names = FALSE)] <- unlist(lapply(cvList, '[[', 'cvdiscreteSL.predict'), use.names = FALSE)
  whichDiscreteSL <- lapply(cvList, '[[', 'cvwhichDiscreteSL')
  library.predict[unlist(folds, use.names = FALSE), ] <- do.call('rbind', lapply(cvList, '[[', 'cvlibrary.predict'))
  coef <- do.call('rbind', lapply(cvList, '[[', 'cvcoef'))
  colnames(coef) <- libraryNames
  
  # put everything together in a list
  out <- list(call = call, 
              AllSL = AllSL, 
              SL.predict = SL.predict, 
              discreteSL.predict = discreteSL.predict, 
              whichDiscreteSL = whichDiscreteSL, 
              library.predict = library.predict, 
              coef = coef, 
              folds = folds, 
              V = V, 
              libraryNames = libraryNames, 
              SL.library = library, 
              method = method, 
              Y = Y)
  class(out) <- "CV.SuperLearner"
  return(out)
}