listWrappers <- function(what = "both") {
	everything <- sort(getNamespaceExports("SuperLearner"))
	if(what == "both") {
		message("All prediction algorithm wrappers in SuperLearner:\n")
		print(everything[grepl(pattern="^[S]L", everything)])
		message("\nAll screening algorithm wrappers in SuperLearner:\n")
		print("All")
		print(everything[grepl(pattern="screen", everything)])
	} else if(what == "SL") {
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
