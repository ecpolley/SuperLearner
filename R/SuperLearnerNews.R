SuperLearnerNews <- function(...) {
	RShowDoc("NEWS", package = "SuperLearner", ...)
}

SuperLearnerDocs <- function(what = 'SuperLearnerR.pdf', ...) {
  if(what == 'NEWS') {
    RShowDoc('NEWS', package = "SuperLearner", ...)
  } else {
    f <- system.file('doc', what, package = 'SuperLearner')
    if (.Platform$OS.type == 'windows')
     shell.exec(f)
     else system(paste(Sys.getenv('R_PDFVIEWER'), f, '&'))
     return(f)
  }
	
}