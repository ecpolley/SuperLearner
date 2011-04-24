trimLogit <- function(x, trim=0.00001) {
	x[x < trim] <- trim
	x[x > (1-trim)] <- (1-trim)
	foo <- log(x/(1-x))
	return(foo)
}