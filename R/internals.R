# Internal functions for SuperLearner package
# 
# Created by Eric Polley on 2011-01-03
# 

# .SL.require() extends the require() function to add my own error messages
.SL.require <- function(package, message = paste('loading required package (', package, ') failed', sep = '')) {
  if(!require(package, character.only = TRUE)) {
    stop(message, call. = FALSE)
  }
  invisible(TRUE)
}

# .createLibrary()
# takes the input from SL.library and sets up the library for SuperLearner()
# SL.library may be a character vector or a list
# return list of prediction algorithms and list of screening algorithms.  If screening algorithms are used, must generate the whichScreen matrix and assign the appropriate row number to the prediction algorithm.  whichScreen[1, ] will always be the full data set, even if no screening algorithms are specified.
# If character vector, only prediction algorithms are allowed.
# for the list, the structure is:
# list(c("predAlg1", "screenAlg1", "screenAlg2", ...), c("predAlg2", "screenAlg1", ...), c("predAlg3"))
# the list contains character vectors, and the scructure of the character vectors is always prediction algorithm first, followed by a list of screening algorithms to be coupled with the prediction algorithm.  If no screening algorithm is given (as in "predAlg3" above) then the algorithm will run on the entire set of variables by default.

.createLibrary <- function(SL.library) {
	if (is.character(SL.library)) { 
		k <- length(SL.library)
		whichScreen <- matrix(1, nrow = 1, ncol = k)
		screenAlgorithm <- "All"
		library <- data.frame(predAlgorithm = SL.library, rowScreen = 1, stringsAsFactors=FALSE)
	} else if (is.list(SL.library)) {
		predNames <- sapply(SL.library, FUN = "[", 1)
		NumberScreen <- (sapply(SL.library, FUN = length) - 1)
		if (sum(NumberScreen == 0) > 0) {
			for(ii in which(NumberScreen == 0)) {
				SL.library[[ii]] <- c(SL.library[[ii]], "All")
				NumberScreen[ii] <- 1
			}
		}
		screenAlgorithmFull <- unlist(lapply(SL.library, FUN="[", -1))
		screenAlgorithm <- unique(screenAlgorithmFull)
		
		library <- data.frame(predAlgorithm = rep(predNames, times=NumberScreen), rowScreen = match(screenAlgorithmFull, screenAlgorithm), stringsAsFactors = FALSE)
	} else {
	  stop('format for SL.library is not recognized')
	}
	
	out <- list(library = library, screenAlgorithm = screenAlgorithm)
	return(out)
}


# Scans over SL.library and loads required packages.  This step is not necessary since most of the wrapper functions also include require() statements, but this helps detect missing packages early.
# Should this be extended to allow the user to pass additional packages in a character vector to check?  might be helpful for the cluster setting.
# addPackages <- c('MASS', 'class', 'nnet', 'ipred')
# sapply(addPackages, function(x) require(force(x), character.only = TRUE))

.check.SL.library <- function(library, addPackages = NULL) {
	if("SL.bayesglm" %in% library) .SL.require('arm', message = 'You have selected bayesglm as a library algorithm but either do not have the arm package installed or it can not be loaded')
	if("SL.cforest" %in% library) .SL.require('party', message = 'You have selected cforest as a library algorithm but either do not have the party package installed or it can not be loaded')
	if("SL.DSA" %in% library) .SL.require('DSA', message = 'You have selected DSA as a library algorithm but either do not have the DSA package installed or it can not be loaded')
	if("SL.gam" %in% library) .SL.require('gam', message = 'You have selected gam as a library algorithm but either do not have the gam package installed or it can not be loaded')
	if("SL.gbm" %in% library) .SL.require('gbm', message = 'You have selected gbm as a library algorithm but either do not have the gbm package installed or it can not be loaded')
	if("SL.glmnet" %in% library)	.SL.require('glmnet', message = 'You have selected glmnet as a library algorithm but either do not have the glmnet package installed or it can not be loaded')
	if("SL.knn" %in% library) .SL.require('class', message = 'You have selected knn as a library algorithm but either do not have the class package installed or it can not be loaded')
	if("SL.logreg" %in% library) .SL.require('LogicReg', message = 'You have selected logreg as a library algorithm but either do not have the LogicReg package installed or it can not be loaded')
	if("SL.nnet" %in% library) .SL.require('nnet', message = 'You have selected nnet as a library algorithm but either do not have the nnet package installed or it can not be loaded')
	if("SL.polymars" %in% library) .SL.require('polspline', message = 'You have selected polymars or polyclass as a library algorithm but either do not have the polspline package installed or it can not be loaded')
	if("SL.randomForest" %in% library) .SL.require('randomForest', message = 'You have selected randomForest as a library algorithm but either do not have the randomForest package installed or it can not be loaded')
	if("SL.ridge" %in% library) .SL.require('MASS', message = 'You have selected lm.ridge as a library algorithm but either do not have the MASS package installed or it can not be loaded')
	if("SL.spls" %in% library) .SL.require('spls', message = 'You have selected spls as a library algorithm but either do not have the spls package installed or it can not be loaded')
	if("SL.step.plr" %in% library) .SL.require('stepPlr', message = 'You have selected step.plr as a library algorithm but either do not have the stepPlr package installed or it can not be loaded')
	if("SL.svm" %in% library) .SL.require('e1071', message = 'You have selected svm as a library algorithm but either do not have the e1071 package installed or it can not be loaded')
	if("SL.ipredbagg" %in% library) .SL.require('ipred', message = 'You have selected bagging as a library algorithm but either do not have the ipred package installed or it can not be loaded')
	if("SL.bart" %in% library) .SL.require('BayesTree', message = 'You have selected bart as a library algorithm but either do not have the BayesTree package installed or it can not be loaded')
	if("SL.gbm" %in% library) .SL.require('gbm', message = 'You have selected gbm as a library algorithm but either do not have the gbm package installed or it can not be loaded')
	if("SL.mars" %in% library) .SL.require('mda', message = 'You have selected mars as a library algorithm but either do not have the mda package installed or it can not be loaded')
	if("SL.earth" %in% library) .SL.require('earth', message = 'You have selected earth as a library algorithm but either do not have the earth package installed or it can not be loaded')
	if("SL.caret" %in% library) .SL.require('caret', message = 'You have selected caret as a library algorithm but either do not have the caret package installed or it can not be loaded')
	if(!is.null(addPackages)) {
	  sapply(addPackages, function(x) require(force(x), character.only = TRUE))
	}
	invisible(TRUE)
}