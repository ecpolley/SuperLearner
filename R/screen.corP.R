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