# merge two superlearners and refit
# Usage:
# Create a SuperLearner.CV.control with validRows given
# Fit two SuperLearners providing the same SuperLearner.CV.control
# Call mergeSuperLearners(oldsl,newsl,Y,method)


mergeSuperLearners <-function(oldsl,newsl,Y,method='method.NNLS',verbose=T){  
  oldsl$Z=cbind(oldsl$Z,newsl$Z)
  oldsl$libraryNames=c(oldsl$libraryNames,newsl$libraryNames)
  oldsl$library.predict=cbind(oldsl$library.predict,newsl$library.predict)
  oldsl$SL.library$library=rbind(oldsl$SL.library$library,newsl$SL.library$library)
  oldsl$errorsInLibrary=c(oldsl$errorsInLibrary,newsl$errorsInLibrary)
  oldsl$errorsInCVLibrary=c(oldsl$errorsInLibrary,newsl$errorsInLibrary)
  
  object=oldsl
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

  
  # Compute new coefficients for the combined SuperLearner
  getCoef <- method$computeCoef(Z = object$Z, Y = Y , libraryNames = object$libraryNames, obsWeights = object$obsWeights, control = object$control, verbose = verbose)
  coef <- getCoef$coef
  names(coef) <- object$libraryNames
  getPred <- method$computePred(predY = object$library.predict, coef = coef, control = object$control)
    
  if(sum(object$errorsInCVLibrary) > 0) {
    getCoef$cvRisk[as.logical(object$errorsInCVLibrary)] <- NA
  }
    
  object$SL.predict=getPred
  object$coef=coef    
  object$cvRisk = getCoef$cvRisk
  object$method=method
  return (object)
}

