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