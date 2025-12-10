#' Control parameters for `SuperLearner()`
#'
#' @param saveFitLibrary Logical. Should the fit for each algorithm be saved in
#' the output from \code{SuperLearner()}.
#' @param saveCVFitLibrary Logical. Should cross-validated fits for each
#' algorithm be saved in the output from \code{SuperLearner()}.
#' @param trimLogit number between 0.0 and 0.5. What level to truncate the
#' logit transformation to maintain a bounded loss function when using the
#' NNloglik method.
#'
#' @returns
#' A list containing the control parameters.
#'
#' @keywords utilities
#'
#' @export
SuperLearner.control <- function(saveFitLibrary = TRUE,
                                 saveCVFitLibrary = FALSE,
                                 trimLogit = 0.001) {
  if (trimLogit > 0.5) {
    warning('trimLogit must be less than 0.5, will replace with trimLogit = 0.001')
    trimLogit <- 0.001
  }

  list(saveFitLibrary = saveFitLibrary, trimLogit = trimLogit, saveCVFitLibrary = saveCVFitLibrary)
}

#' Control parameters for the cross validation steps in \code{SuperLearner()}
#'
#' @param V Integer. Number of splits for the V-fold cross-validation step. The
#' default is 10. In most cases, between 10 and 20 splits works well.
#' @param stratifyCV Logical. Should the data splits be stratified by a binary
#' response? Attempts to maintain the same ratio in each training and
#' validation sample.
#' @param shuffle Logical. Should the rows of \code{X} be shuffled before
#' creating the splits.
#' @param validRows A List. Use this to pass pre-specified rows for the sample
#' splits. The length of the list should be \code{V} and each entry in the list
#' should contain a vector with the row numbers of the corresponding validation
#' sample.
#'
#' @returns
#' A list containing the control parameters.
#'
#' @keywords utilities

#' @export `SuperLearner.CV.control`
SuperLearner.CV.control <- function(V = 10L, stratifyCV = FALSE, shuffle = TRUE, validRows = NULL){
  # make sure V is an integer
  V <- as.integer(V)

  # Checks for user supplied validRows is present:
  if (!is.null(validRows)) {
    if (!is.list(validRows)) {
      stop('validRows must be a list of length V containing the row numbers for the corresponding validation set')
    }

    if (!identical(V, length(validRows))) {
      stop('V and length(validRows) must be identical')
    }
  }
  list(V = V, stratifyCV = stratifyCV, shuffle = shuffle, validRows = validRows)
}

