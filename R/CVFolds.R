# create a list of row numbers for the V-fold cross validation.
# based on sample size N, id, Y, and cvControl
# 
# inside cvControl:
# V : number of folds
# stratifyCV : if Y is binary, stratify folds to try and keep proportion constant
# shuffle : should the rows of X,Y be shuffled since the split function is deterministic

# created by Eric Polley on 2011-01-18.

CVFolds <- function(N, id, Y, cvControl){
  # validRows would be a user specified list of row numbers for the validation sets
	if(!is.null(cvControl$validRows)) {
	  return(cvControl$validRows)
	}
	stratifyCV <- cvControl$stratifyCV
	shuffle <- cvControl$shuffle
	V <- cvControl$V
	
	if(!stratifyCV) {
		if(shuffle) {
			if(is.null(id)) {
				validRows <- split(sample(1:N), rep(1:V, length=N))
			} else {
				n.id <- length(unique(id))
				id.split <- split(sample(1:n.id), rep(1:V, length=n.id))
				validRows <- vector("list", V)
				for(v in seq(V)) {
					validRows[[v]] <- which(id %in% unique(id)[id.split[[v]]])
				}
			}
		} else {
			if(is.null(id)) {
				validRows <- split(1:N, rep(1:V, length=N))
			} else {
				n.id <- length(unique(id))
				id.split <- split(1:n.id, rep(1:V, length=n.id))
				validRows <- vector("list", V)
				for(v in seq(V)) {
					validRows[[v]] <- which(id %in% unique(id)[id.split[[v]]])
				}
			}
		}
	} else {
		if(length(unique(Y)) != 2) {
			stop("stratifyCV only implemented for binary Y")
		}
		if(sum(Y) < V | sum(!Y) < V) {
			stop("number of (Y=1) or (Y=0) is less than the number of folds")
		}
		if(shuffle) {
			if(is.null(id)) {
			  wiY0 <- which(Y == 0)
			  wiY1 <- which(Y == 1)
			  rowsY0 <- split(sample(wiY0), rep(1:V, length=length(wiY0)))
			  rowsY1 <- split(sample(wiY1), rep(1:V, length=length(wiY1)))
        validRows <- vector("list", length = V)
        names(validRows) <- paste(seq(V))
        for(vv in seq(V)) {
          validRows[[vv]] <- c(rowsY0[[vv]], rowsY1[[vv]])
        }
			} else {
				stop("stratified sampling with id not currently implemented")
			}
		} else {
			if(is.null(id)) {
				within.split <- suppressWarnings(tapply(1:N, INDEX = Y, FUN = split, rep(1:V)))
				validRows <- vector("list", length = V)
				names(validRows) <- paste(seq(V))
				for(vv in seq(V)) {
					validRows[[vv]] <- c(within.split[[1]][[vv]], within.split[[2]][[vv]])
				}
			} else {
				stop("stratified sampling with id not currently implemented")
			}
		}
	}	
	return(validRows)
}
