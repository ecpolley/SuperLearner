screen.SIS <- function (Y, X, family, nsis = NULL, tune.method = "bic", type.measure = "deviance", minScreen = 5, ...) 
{
  .SL.require('SIS', "you have selected SIS as a library algorithm but do not have the SIS package installed")
  if (!is.matrix(X)) {
		warning("X is not a matrix, screen.SIS will convert to matrix for variable screening")
		Xmat <- as.matrix(X)
  }
	if (family$family == "gaussian") {
		fitSIS <- SIS::SIS(x = Xmat, y = Y, family = family$family, nsis = nsis, tune = tune.method, type.measure = type.measure)
    }
    if (family$family == "binomial") {
		fitSIS <- SIS::SIS(x = Xmat, y = Y, family = family$family, nsis = nsis, tune = tune.method, type.measure = type.measure)
	}
	whichVariable <- (1:ncol(X) %in% fitSIS$ix)
	# check more than minScreen in screened set
	if(sum(whichVariable) < minScreen) {
		warning("fewer than minScreen variables in screen.SIS, using initial ranking")
	}
    return(whichVariable)
}