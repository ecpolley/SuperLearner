# merge two superlearners and refit
# Usage:
# Create a SuperLearner.CV.control with validRows given
# Fit two SuperLearners with the same SuperLearner.CV.control
# Call mergeSuperLearners(firstsl,secondsl,Y,method)

# ## simulate data
# set.seed(23432)
# ## training set
# n <- 500
# p <- 50
# X <- matrix(rnorm(n*p), nrow = n, ncol = p)
# colnames(X) <- paste("X", 1:p, sep="")
# X <- data.frame(X)
# Y <- X[, 1] + sqrt(abs(X[, 2] * X[, 3])) + X[, 2] - X[, 3] + rnorm(n)
# 
# ## test set
# m <- 1000
# newX <- matrix(rnorm(m*p), nrow = m, ncol = p)
# colnames(newX) <- paste("X", 1:p, sep="")
# newX <- data.frame(newX)
# newY <- newX[, 1] + sqrt(abs(newX[, 2] * newX[, 3])) + newX[, 2] -
#   newX[, 3] + rnorm(m)
# 
# # generate Library and run Super Learner
# SL.library <- c("SL.glm", "SL.mean")
# test1 <- SuperLearner(Y = Y, X = X, newX = newX, SL.library = SL.library,
#                      verbose = TRUE, method = "method.NNLS")
# test1
# 
# SL.library <- c("SL.glmnet", "SL.loess")
# 
# ctrl=SuperLearner.CV.control(V = 10,validRows = test1$validRows)
# test2 <- SuperLearner(Y = Y, X = X, newX = newX, SL.library = SL.library,cvControl = ctrl,
#                      verbose = TRUE, method = "method.NNLS")
# test2
# merged=mergeSuperLearners(test,test2,Y = Y,X=X)
# merged


mergeSuperLearners <-function(firstsl,secondsl,Y,X,method='method.NNLS',obsWeights=NULL,verbose=T){    
  # combine the Z matrices, libraryNames, predictions, errors
  firstsl$Z=cbind(firstsl$Z,secondsl$Z)
  firstsl$libraryNames=c(firstsl$libraryNames,secondsl$libraryNames)
  firstsl$library.predict=cbind(firstsl$library.predict,secondsl$library.predict)
  firstsl$SL.library$library=rbind(firstsl$SL.library$library,secondsl$SL.library$library)
  firstsl$errorsInLibrary=c(firstsl$errorsInLibrary,secondsl$errorsInLibrary)
  firstsl$errorsInCVLibrary=c(firstsl$errorsInLibrary,secondsl$errorsInLibrary)
  
  N <- dim(X)[1L]    
  if(is.null(obsWeights)) {
    obsWeights <- rep(1, N)
  }
  if(!identical(length(obsWeights), N)) {
    stop("obsWeights vector must have the same dimension as Y")
  }
  
  object=firstsl
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
  getCoef <- method$computeCoef(Z = object$Z, Y = Y , libraryNames = object$libraryNames, obsWeights = obsWeights, control = object$control, verbose = verbose)
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

