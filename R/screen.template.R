# Pass all variables:
All <- function(X, ...) {
	rep.int(TRUE, ncol(X))
}

# screen functions must return a logical vector of length ncol(X)
screen.template <- function (Y, X, family, obsWeights, id, ...) 
{
  # .SL.require('pkg')
  if (family$family == "gaussian") {
	
  }
  if (family$family == "binomial") {

  }
  whichVariable <- rep(TRUE, ncol(X))
  return(whichVariable)
}

write.screen.template <- function(file = '', ...) {
  cat('screen.template <- function(Y, X, family, obsWeights, id, ...) {\n  # load required packages\n  # require(\'pkg\')\n  if (family$family == \'gaussian\') {\n    \n  }\n  if (family$family == \'binomial\') {\n  \n  }\n  # whichVariable is a logical vector,\n  # TRUE indicates variable will be used\n  whichVariable <- rep(TRUE, ncol(X))\n  return(whichVariable)\n}', file = file, ...)
}