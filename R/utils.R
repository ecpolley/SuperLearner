# subset and refit superlearners
subsetLearners<-function(object,Y,selectedLearners,method='method.NNLS',verbose=T) {
  
  if(is.character(method)) {
    if(exists(method, mode = 'list')) {
      method <- get(method, mode = 'list')
    } else if(exists(method, mode = 'function')) {
      method <- get(method, mode = 'function')()
    }
  } else if(is.function(method)) {
    method <- method()
  }
  
  getCoef <- method$computeCoef(Z = object$Z[,selectedLearners], Y = Y , libraryNames = object$libraryNames[selectedLearners], obsWeights = object$obsWeights, control = object$control, verbose = verbose)
  coef <- getCoef$coef
  names(coef) <- object$libraryNames[selectedLearners]
  getPred <- method$computePred(predY = object$library.predict[,selectedLearners], coef = coef, control = control)
  
  object$libraryNames=object$libraryNames[selectedLearners]
  object$SL.library$library=object$SL.library$library[selectedLearners,]
  object$SL.predict=getPred
  object$coef=coef
  object$library.predict=object$library.predict[,selectedLearners]
  object$Z=object$Z[,selectedLearners]
  object$cvRisk = getCoef$cvRisk
  return(object)
}

# merge two superlearners and refit
mergeSuperLearners <-function(oldsl,newsl,method='method.NNLS',Y,verbose=T){  
  oldsl$Z=cbind(oldsl$Z,newsl$Z)
  oldsl$libraryNames=c(oldsl$libraryNames,newsl$libraryNames)
  oldsl$library.predict=cbind(oldsl$library.predict,newsl$library.predict)
  oldsl$SL.library$library=rbind(oldsl$SL.library$library,newsl$SL.library$library)
  
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
  
  getCoef <- method$computeCoef(Z = object$Z, Y = Y , libraryNames = object$libraryNames, obsWeights = 1, control = object$control, verbose = verbose)
  coef <- getCoef$coef
  names(coef) <- object$libraryNames
  getPred <- method$computePred(predY = object$library.predict, coef = coef, control = object$control)
    
  object$SL.predict=getPred
  object$coef=coef    
  object$cvRisk = getCoef$cvRisk
  return (object)
}