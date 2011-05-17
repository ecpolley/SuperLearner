predict.SuperLearner <- function(object, newdata, family, X = NULL, Y = NULL, ...) {
  if(missing(newdata)) {
    out <- list(pred = object$SL.predict, library.predict = object$library.predict)
    return(out)
  }
  
  k <- length(object$libraryNames)
  predY <- matrix(NA, nrow = nrow(newdata), ncol = k)
  colnames(predY) <- object$libraryNames
  for(mm in seq(k)) {
    predY[, mm] <- do.call('predict', list(object = object$fitLibrary[[mm]], 
                                    newdata = subset(newdata, select = object$whichScreen[object$SL.library$library[mm, 2], ]),
                                    family = object$family,
                                    X = if(is.null(X)) NULL else subset(X, select = object$whichScreen[object$SL.library$library[mm, 2], ]),
                                    Y = Y,
                                    ...))
  }

  getPred <- object$method$computePred(predY = predY, coef = object$coef, control = object$control)
  out <- list(pred = getPred, library.predict = predY)
  return(out)
}

