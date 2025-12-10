#' @exportS3Method print SuperLearner
print.SuperLearner <- function(x, ...) {
	cat("\nCall: ", deparse(x$call, width.cutoff = .9*getOption("width")), "\n\n", fill = getOption("width"))
	print(cbind(Risk = x$cvRisk, Coef = x$coef))
}

#' @exportS3Method stats::coef SuperLearner
coef.SuperLearner <- function(object, ...) {
	object$coef
}

#' @exportS3Method print CV.SuperLearner
print.CV.SuperLearner <- function(x, ...) {
	cat("\nCall: ", deparse(x$call, width.cutoff = .9*getOption("width")), "\n\n", fill = getOption("width"))
	cat("Cross-validated predictions from the SuperLearner:  SL.predict \n\nCross-validated predictions from the discrete super learner (cross-validation selector):  discreteSL.predict \n\nWhich library algorithm was the discrete super learner:  whichDiscreteSL \n\nCross-validated prediction for all algorithms in the library:  library.predict\n")
}

#' @exportS3Method stats::coef CV.SuperLearner
coef.CV.SuperLearner <- function(object, ...) {
	object$coef
}
