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