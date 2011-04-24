# control functions for SuperLearner()
# 
# Created by Eric Polley on 2011-01-03.
# 
SuperLearner.control <- function(saveFitLibrary = TRUE, trimLogit = 0.001) {
  if(trimLogit > 0.5) {
    warning('trimLogit must be less than 0.5, will replace with trimLogit = 0.001')
    trimLogit <- 0.001
  }
  list(saveFitLibrary = saveFitLibrary, trimLogit = trimLogit)
}

SuperLearner.CV.control <- function(V = 10L, stratifyCV = FALSE, shuffle = TRUE, validRows = NULL){
  # make sure V is an integer
  V <- as.integer(V)
  
  # Checks for user supplied validRows is present:
  if(!is.null(validRows)) {
    if(!is.list(validRows)) {
      stop('validRows must be a list of length V containing the row numbers for the corresponding validation set')
    }
    if(!identical(V, length(validRows))) {
      stop('V and length(validRows) must be identical')
    }
  }
  list(V = V, stratifyCV = stratifyCV, shuffle = shuffle, validRows = validRows)
}

