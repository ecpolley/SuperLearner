print.SuperLearner <- function(x, ...) {
	cat("\nCall: ", deparse(x$call, width.cutoff = .9*getOption("width")), "\n\n", fill = getOption("width"))
	print(cbind(Risk = x$cvRisk, Coef = x$coef))
}

coef.SuperLearner <- function(object, ...) {
	object$coef
}

print.CV.SuperLearner <- function(x, ...) {
	cat("\nCall: ", deparse(x$call, width.cutoff = .9*getOption("width")), "\n\n", fill = getOption("width"))
	cat("Cross-validated predictions from the SuperLearner:  SL.predict \n\nCross-validated predictions from the discrete super learner (cross-validation selector):  discreteSL.predict \n\nWhich library algorithm was the discrete super learner:  whichDiscreteSL \n\nCross-validated prediction for all algorithms in the library:  library.predict\n")
}

coef.CV.SuperLearner <- function(object, ...) {
	object$coef
}
