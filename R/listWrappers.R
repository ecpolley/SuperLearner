#' List all wrapper functions in SuperLearner
#'
#' @param what What list to return. Can be \code{both} for both prediction
#' algorithms and screening algorithms, \code{SL} for the prediction
#' algorithms, \code{screen} for the screening algorithms, \code{method} for
#' the estimation method details, or anything else will return a list of all
#' (exported) functions in the \code{SuperLearner} package. Additional wrapper
#' functions are available at
#' \url{https://github.com/ecpolley/SuperLearnerExtra}.
#' @returns
#' Invisible character vector with all exported functions in the
#' \pkg{SuperLearner} package
#'
#' @author Eric C Polley \email{epolley@@uchicago.edu}
#'
#' @seealso \code{\link{SuperLearner}}
#'
#' @keywords utilities
#' @examples
#'
#' listWrappers(what = "SL")
#' listWrappers(what = "screen")

#' @export
listWrappers <- function(what = "both") {
	everything <- sort(getNamespaceExports("SuperLearner"))
	if (what == "both") {
		message("All prediction algorithm wrappers in SuperLearner:\n")
		print(everything[grepl(pattern="^[S]L", everything)])
		message("\nAll screening algorithm wrappers in SuperLearner:\n")
		print("All")
		print(everything[grepl(pattern="screen", everything)])
	} else if (what == "SL") {
		message("All prediction algorithm wrappers in SuperLearner:\n")
		print(everything[grepl(pattern="^[S]L", everything)])
	} else if(what == "screen") {
		message("All screening algorithm wrappers in SuperLearner:\n")
		print("All")
		print(everything[grepl(pattern="screen", everything)])
	} else if(what == 'method') {
	  message("All methods in SuperLearner package:\n")
		print(everything[grepl(pattern="^method", everything)])
	} else {
		message("All functions in SuperLearner:\n")
		print(everything)
	}
	invisible(everything)
}
