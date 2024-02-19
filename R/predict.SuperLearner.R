#' Predict method for SuperLearner object
#'
#' Obtains predictions on a new data set from a SuperLearner fit.  May require
#' the original data if one of the library algorithms uses the original data in
#' its predict method.
#'
#' If \code{newdata} is omitted the predicted values from \code{object} are
#' returned.  Each algorithm in the Super Learner library needs to have a
#' corresponding prediction function with the ``predict.'' prefixed onto the
#' algorithm name (e.g. \code{predict.SL.glm} for \code{SL.glm}).
#'
#' @param object Fitted object from \code{SuperLearner}
#' @param newdata New X values for prediction
#' @param X Original data set used to fit \code{object}, if needed by fit object.
#' @param Y Original outcome used to fit \code{object}, if needed by fit object.
#' @param onlySL Logical. If TRUE, only compute predictions for algorithms with
#' non-zero coefficients in the super learner object. Default is FALSE
#' (computes predictions for all algorithms in library).
#' @param \dots Additional arguments passed to the \code{predict.SL.*}
#' functions
#'
#' @return \item{pred}{ Predicted values from Super Learner fit}
#' \item{library.predict}{ Predicted values for each algorithm in library}
#'
#' @author Eric C Polley \email{epolley@uchicago.edu}
#'
#' @seealso \code{\link{SuperLearner}}
#'
#' @keywords models
predict.SuperLearner <- function(object, newdata, X = NULL, Y = NULL,
                                 onlySL = FALSE, ...) {
  if (missing(newdata)) {
    out <- list(pred = object$SL.predict, library.predict = object$library.predict)
    return(out)
  }
  if (!object$control$saveFitLibrary) {
    stop("This SuperLearner fit was created using control$saveFitLibrary = FALSE, so new predictions cannot be made.")
  }

  k <- length(object$libraryNames)
  predY <- matrix(NA, nrow = nrow(newdata), ncol = k)
  colnames(predY) <- object$libraryNames
  if (onlySL) {
    whichLibrary <- which(object$coef > 0)
    predY <- matrix(0, nrow = nrow(newdata), ncol = k)
    for (mm in whichLibrary) {
      newdataMM <- subset(newdata,
                          select = object$whichScreen[object$SL.library$library[mm, 2], ])
      family <- object$family
      XMM <- if (is.null(X)) {
        NULL
      } else {
        subset(X, select = object$whichScreen[object$SL.library$library[mm, 2], ])
      }
      predY[, mm] <- do.call('predict', list(object = object$fitLibrary[[mm]],
                                             newdata = newdataMM,
                                             family = family,
                                             X = XMM,
                                             Y = Y,
                                             ...))
    }
    getPred <- object$method$computePred(predY = predY, coef = object$coef,
                                         control = object$control)
    out <- list(pred = getPred, library.predict = predY)
  } else {
    for (mm in seq(k)) {
      newdataMM <- subset(newdata,
                          select = object$whichScreen[object$SL.library$library[mm, 2], ])
      family <- object$family
      XMM <- if (is.null(X)) {
        NULL
      } else {
        subset(X, select = object$whichScreen[object$SL.library$library[mm, 2], ])
      }
      predY[, mm] <- do.call('predict', list(object = object$fitLibrary[[mm]],
                                             newdata = newdataMM,
                                             family = family,
                                             X = XMM,
                                             Y = Y,
                                             ...))
    }
    getPred <- object$method$computePred(predY = predY, coef = object$coef,
                                         control = object$control)
    out <- list(pred = getPred, library.predict = predY)
  }
  return(out)
}
