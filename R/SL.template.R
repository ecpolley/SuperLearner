#' Wrapper functions for prediction algorithms in SuperLearner
#' @aliases SL.caret.rpart SL.gbm SL.ipredbagg SL.knn SL.leekasso SL.loess SL.logreg SL.nnet SL.nnls SL.polymars SL.randomForest SL.ridge SL.rpart SL.rpartPrune SL.step SL.step.forward SL.step.interaction SL.stepAIC SL.svm
#'
#' @description
#' Template function for SuperLearner prediction wrappers and built in options.
#'
#' @param Y The outcome in the training data set. Must be a numeric vector.
#' @param X The predictor variables in the training data set, usually a data.frame.
#' @param newX The predictor variables in the validation data set. The
#' structure should match X.
#' @param family Either [gaussian()] or [binomial()] to
#' describe the error distribution. Link function information will be ignored.
#' @param obsWeights Optional observation weights.
#' @param id Optional cluster identification variable.
#' @param \dots For SL wrappers, other remaining arguments. For `write.SL.template()`, arguments passed to [cat()].
#' @param file A connection, or a character string naming a file to print to.
#' Passed to [cat()].
#'
#' @returns
#' SL wrappers return a list with two elements:
#' \item{pred}{ The predicted values for the
#' rows in \code{newX}. }
#' \item{fit}{ A list. Contains all objects necessary to
#' get predictions for new observations from specific algorithm. }
#'
#' @author Eric C Polley \email{epolley@@uchicago.edu}
#'
#' @seealso \code{\link{SuperLearner}}
#'
#' @keywords utilities
#'
#' @examples
#' write.SL.template(file = '')
#'

#' @export `SL.template`
SL.template <- function(Y, X, newX, family, obsWeights, id, ...) {
  if (family$family == "gaussian") {
    # insert estimation and prediction function
  }
  else if (family$family == "binomial") {
    # insert estimation and prediction function
  }
  # pred returns predicted responses (on the scale of the outcome)
  pred <- numeric()
  # fit returns all objects needed for predict.SL.template
  # fit <- list(object = )
  fit <- vector("list", length=0)
  class(fit) <- c("SL.template")
  out <- list(pred = pred, fit = fit)
  return(out)
}

#' @export `write.SL.template`
#' @rdname SL.template
write.SL.template <- function(file = '', ...) {
  cat('SL.template <- function(Y, X, newX, family, obsWeights, id, ...) {\n  # load required packages\n  # require(\'pkg\')\n  if (family$family == \'gaussian\') {\n  \n  }\n elseif (family$family == \'binomial\') {\n  \n  }\n  # pred is the predicted responses for newX (on the scale of the outcome)\n  pred <- numeric()\n  # fit returns all objects needed for predict.SL.template\n  fit <- list(object = )\n  # declare class of fit for predict.SL.template\n  class(fit) <- \'SL.template\'\n  # return a list with pred and fit\n  out <- list(pred = pred, fit = fit)\n  return(out)\n}', file = file, ...)
}

