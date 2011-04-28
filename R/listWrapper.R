listWrappers <- function(what = "both") {
	if(what == "both") {
		message("All prediction algorithm wrappers in SuperLearner:\n")
		print(ls("package:SuperLearner", pattern="^[S]L"))
		message("\nAll screening algorithm wrappers in SuperLearner:\n")
		print("All")
		print(ls("package:SuperLearner", pattern="screen"))
	} else if(what == "SL") {
		message("All prediction algorithm wrappers in SuperLearner:\n")
		print(ls("package:SuperLearner", pattern="^[S]L"))
	} else if(what == "screen") {
		message("All screening algorithm wrappers in SuperLearner:\n")
		print("All")
		print(ls("package:SuperLearner", pattern="screen"))
	} else if(what == 'method') {
	  message("All methods in SuperLearner package:\n")
		print(ls("package:SuperLearner", pattern="^method"))
	} else {
		message("All functions in SuperLearner:\n")
		print(ls("package:SuperLearner"))
	}
	invisible(ls("package:SuperLearner"))
}