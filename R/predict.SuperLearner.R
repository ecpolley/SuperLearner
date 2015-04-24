predict.SuperLearner <- function(object, newdata, X = NULL, Y = NULL, onlySL = FALSE, ...) {
  if(missing(newdata)) {
    out <- list(pred = object$SL.predict, library.predict = object$library.predict)
    return(out)
  }
  if (!object$control$saveFitLibrary) {
    stop("This SuperLearner fit was created using control$saveFitLibrary = FALSE, so new predictions cannot be made.")
  }
  
  k <- length(object$libraryNames)
  predY <- matrix(NA, nrow = nrow(newdata), ncol = k)
  colnames(predY) <- object$libraryNames
  if(onlySL) {
    whichLibrary <- which(object$coef > 0)
    predY <- matrix(0, nrow = nrow(newdata), ncol = k)
    for(mm in whichLibrary) {
      newdataMM <- subset(newdata, select = object$whichScreen[object$SL.library$library[mm, 2], ])
      family <- object$family
      XMM <- if(is.null(X)) NULL else subset(X, select = object$whichScreen[object$SL.library$library[mm, 2], ])
      predY[, mm] <- do.call('predict', list(object = object$fitLibrary[[mm]], 
                                    newdata = newdataMM,
                                    family = family,
                                    X = XMM,
                                    Y = Y,
                                    ...))
      }
    getPred <- object$method$computePred(predY = predY, coef = object$coef, control = object$control)
    out <- list(pred = getPred, library.predict = predY)
  } else {
    for(mm in seq(k)) {
      newdataMM <- subset(newdata, select = object$whichScreen[object$SL.library$library[mm, 2], ])
      family <- object$family
      XMM <- if(is.null(X)) NULL else subset(X, select = object$whichScreen[object$SL.library$library[mm, 2], ])
      predY[, mm] <- do.call('predict', list(object = object$fitLibrary[[mm]], 
                                    newdata = newdataMM,
                                    family = family,
                                    X = XMM,
                                    Y = Y,
                                    ...))
      }
    getPred <- object$method$computePred(predY = predY, coef = object$coef, control = object$control)
    out <- list(pred = getPred, library.predict = predY)
  }
  return(out)
}

