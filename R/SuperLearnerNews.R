#' Show the NEWS and documentation files for the SuperLearner package
#'
#' Show the NEWS file of the SuperLearner package. The function is simply a
#' wrapper for [utils::RShowDoc()].
#'
#' @param what specify what document to open. Currently supports the NEWS file
#' and the PDF files 'SuperLearner.pdf' and 'SuperLearnerR.pdf'.
#' @param \dots additional arguments passed to [utils::RShowDoc()].
#'
#' @returns
#' A invisible character string given the path to the SuperLearner NEWS or documentation file.
#'
#' @keywords utilities

#' @export
SuperLearnerDocs <- function(what = 'SuperLearnerR.pdf', ...) {
  if (what == 'NEWS') {
    utils::RShowDoc('NEWS', package = "SuperLearner", ...)
  }
  else {
    f <- system.file('doc', what, package = 'SuperLearner')
    if (.Platform$OS.type == 'windows') shell.exec(f)
    else system(paste(Sys.getenv('R_PDFVIEWER'), f, '&'))

    return(f)
  }
}

#' @export
#' @rdname SuperLearnerDocs
SuperLearnerNews <- function(...) {
	utils::RShowDoc("NEWS", package = "SuperLearner", ...)
}

