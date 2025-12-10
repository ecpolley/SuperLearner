#' Screening algorithms for SuperLearner
#' @name screen
#'
#' @description
#' Screening algorithms for \code{SuperLearner} to be used with
#' \code{SL.library}.
#'
#' @aliases write.screen.template screen.template All screen.randomForest
#' screen.SIS screen.ttest screen.corP screen.corRank screen.glmnet
#'
#' @param file A connection, or a character string naming a file to print to.
#' Passed to \code{\link{cat}}.
#' @param \dots Additional arguments passed to \code{\link{cat}}
#'
#' @returns
#' A logical vector with the length equal to the
#' number of columns in \code{X}.  TRUE indicates the variable (column of X)
#' should be included.
#'
#' @author Eric C Polley \email{polley.eric@@mayo.edu}
#'
#' @seealso \code{\link{SuperLearner}}
#'
#' @keywords utilities
#'
#' @examples
#' write.screen.template(file = '')
#'

#' @export `write.screen.template`
#' @rdname screen
write.screen.template <- function(file = '', ...) {
  cat('screen.template <- function(Y, X, family, obsWeights, id, ...) {\n  # load required packages\n  # library(\'pkg\')\n  if (family$family == \'gaussian\') {\n    \n  }\n  if (family$family == \'binomial\') {\n  \n  }\n  # whichVariable is a logical vector,\n  # TRUE indicates variable will be used\n  whichVariable <- rep(TRUE, ncol(X))\n  return(whichVariable)\n}', file = file, ...)
}

#' @export
# screen functions must return a logical vector of length ncol(X)
screen.template <- function(Y, X, family, obsWeights, id, ...)
{
  # library('pkg')
  if (family$family == "gaussian") {

  }
  if (family$family == "binomial") {

  }
  whichVariable <- rep(TRUE, ncol(X))
  return(whichVariable)
}

#' @export
# Pass all variables:
All <- function(X, ...) {
  rep.int(TRUE, ncol(X))
}

#' @export
screen.corP <- function(Y, X, family, obsWeights, id, method = 'pearson', minPvalue = 0.1, minscreen = 2, ...) {
  listp <- apply(X, 2, function(x, Y, method) {
    ifelse(var(x) <= 0, 1, cor.test(x, y = Y, method = method)$p.value)
  }, Y = Y, method = method)
  whichVariable <- (listp <= minPvalue)
  if (sum(whichVariable) < minscreen) {
    warning('number of variables with p value less than minPvalue is less than minscreen')
    whichVariable[rank(listp) <= minscreen] <- TRUE
  }
  return(whichVariable)
}

#' @export
screen.corRank <- function(Y, X, family, method = 'pearson', rank = 2, ...) {
  # if(rank > ncol(X)) {
  #     rank <- ncol(X)
  # }
  # Don't really need that check, but might want to add a warning message
  listp <- apply(X, 2, function(x, Y, method) {
    ifelse(var(x) <= 0, 1, cor.test(x, y = Y, method = method)$p.value)
  }, Y = Y, method = method)
  whichVariable <- (rank(listp) <= rank)
  return(whichVariable)
}

#' @export
screen.glmnet <- function(Y, X, family, alpha = 1, minscreen = 2, nfolds = 10, nlambda = 100,  ...) {
  .SL.require('glmnet')
  if(!is.matrix(X)) {
    X <- model.matrix(~ -1 + ., X)
  }
  fitCV <- glmnet::cv.glmnet(x = X, y = Y, lambda = NULL, type.measure = 'deviance', nfolds = nfolds, family = family$family, alpha = alpha, nlambda = nlambda)
  whichVariable <- (as.numeric(coef(fitCV$glmnet.fit, s = fitCV$lambda.min))[-1] != 0)
  # the [-1] removes the intercept
  if (sum(whichVariable) < minscreen) {
    warning("fewer than minscreen variables passed the glmnet screen, increased lambda to allow minscreen variables")
    sumCoef <- apply(as.matrix(fitCV$glmnet.fit$beta), 2, function(x) sum((x != 0)))
    newCut <- which.max(sumCoef >= minscreen)
    whichVariable <- (as.matrix(fitCV$glmnet.fit$beta)[, newCut] != 0)
  }
  return(whichVariable)
}

#' @export
screen.randomForest <- function(Y, X, family, nVar = 10, ntree = 1000,
                                mtry = ifelse(family$family == "gaussian", floor(sqrt(ncol(X))), max(floor(ncol(X)/3), 1)),
                                nodesize = ifelse(family$family == "gaussian", 5, 1), maxnodes = NULL, ...)
{
  .SL.require('randomForest')
  if (family$family == "gaussian") {
    rank.rf.fit <- randomForest::randomForest(Y ~ ., data = X, ntree = ntree, mtry = mtry, nodesize = nodesize, keep.forest = FALSE, maxnodes = maxnodes)
  }
  else if (family$family == "binomial") {
    rank.rf.fit <- randomForest::randomForest(as.factor(Y) ~ ., data=X, ntree = ntree, mtry = mtry, nodesize = nodesize, keep.forest = FALSE, maxnodes = maxnodes)
  }
  whichVariable <- (rank(-rank.rf.fit$importance) <= nVar)
  return(whichVariable)
}

#' @export
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

#' @export
screen.ttest <- function(Y, X, family, obsWeights, id, rank = 2, ...) {
  # implemented with colttests from the genefilter package
  .SL.require('genefilter')
  if (family$family == "gaussian") {
    stop('t-test screening undefined for gaussian family, look at screen.corP or screen.corRank')
  }
  if (family$family == "binomial") {
    listP <- genefilter::colttests(x = as.matrix(X), fac = as.factor(Y), tstatOnly = FALSE)$p.value
  }
  whichVariable <- (rank(listP) <= rank)
  return(whichVariable)
}