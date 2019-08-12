## gam{gam}
## generalized additive models (degree = 2)
# functions considers any variable with more than 4 (change with cts.num) unique values to be continuous and able to be in smoothing splines. 
# easy to add additional algorithms with different degrees
# SL.gam.3 <- function(...,deg.gam = 3) SL.gam(..., deg.gam = deg.gam)

SL.gam <- function(Y, X, newX, family, obsWeights, deg.gam = 2, cts.num = 4, ...) {
	# using require instead of requireNamespace() to allow the formula to parse correctly with s(), gam::s() doesn't work, is not recognized as a special function
  if(!require('gam')) {stop("SL.gam requires the gam package, but it isn't available")} 
  if("mgcv" %in% loadedNamespaces()) warning("mgcv and gam packages are both in use. You might see an error because both packages use the same function names.")
  # create the formula for gam with a spline for each continuous variable
  cts.x <- apply(X, 2, function(x) (length(unique(x)) > cts.num))
  if (sum(!cts.x) > 0) { 
    gam.model <- as.formula(paste("Y~", paste(paste("s(", colnames(X[, cts.x, drop = FALSE]), ",", deg.gam,")", sep=""), collapse = "+"), "+", paste(colnames(X[, !cts.x, drop=FALSE]), collapse = "+")))
  } else {
    gam.model <- as.formula(paste("Y~", paste(paste("s(", colnames(X[, cts.x, drop = FALSE]), ",", deg.gam, ")", sep=""), collapse = "+")))
  }
  # fix for when all variables are binomial
  if (sum(!cts.x) == length(cts.x)) {
    gam.model <- as.formula(paste("Y~", paste(colnames(X), collapse = "+"), sep = ""))
  }
  fit.gam <- gam::gam(gam.model, data = X, family = family, control = gam::gam.control(maxit = 50, bf.maxit = 50), weights = obsWeights)
  if(packageVersion('gam') >= 1.15) {
    pred <- gam::predict.Gam(fit.gam, newdata = newX, type = "response") # updated gam class in version 1.15
  } else {
    stop("This SL.gam wrapper requires gam version >= 1.15, please update the gam package with 'update.packages('gam')'")
  }
  fit <- list(object = fit.gam)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.gam")
  return(out)
}

predict.SL.gam <- function(object, newdata, ...){
  .SL.require('gam')
  if(packageVersion('gam') >= 1.15) {
    pred <- gam::predict.Gam(object = object$object, newdata = newdata, type = "response") # updated gam class in version 1.15
  } else {
    stop("This SL.gam wrapper requires gam version >= 1.15, please update the gam package with 'update.packages('gam')'")
  }
  
  return(pred)
}

