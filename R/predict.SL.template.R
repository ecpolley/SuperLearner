#' Wrapper functions for computing predictions from SL learners
#'
#' @inheritParams SL.template
#' @param object an object resulting from the fitting function.
#' @param newdata a data frame of predictors for which to compute predictions.
#' @param family Either [gaussian()] or [binomial()] to describe the error distribution. Link function information will be ignored.
#' @param X The predictor variables in the training data set, usually a data.frame.
#' @param Y The outcome in the training data set. Must be a numeric vector.
#' @param \dots For `predict.SL` wrappers, other remaining arguments. For `write.predict.SL.template()`, arguments passed to [cat()].
#'
#' @returns
#' `predict.SL` wrappers return a vector of predictions for each unit in `newdata`.
#'
#' @keywords utilities
#'
#' @examples
#' write.predict.SL.template(file = '')

#' @export `predict.SL.template`
predict.SL.template <- function(object, newdata, family, X = NULL, Y = NULL,...) {
  # insert prediction function
  pred <- numeric()
  return(pred)
}

#' @export `write.predict.SL.template`
#' @rdname predict.SL.template
write.predict.SL.template <- function(file = '', ...) {
  cat('predict.SL.template <-  function(object, newdata, family, X = NULL, Y = NULL,...) {\n  # insert prediction function\n  # pred <- numeric()\n  return(pred)\n}', file = file, ...)
}

