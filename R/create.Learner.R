#' Factory for learner wrappers
#'
#' Create custom learners and/or a sequence of learners with hyperparameter
#' combinations defined over a grid.
#'
#' @param base_learner Character string of the learner function that will be
#'   customized.
#' @param params List with parameters to customize.
#' @param tune List of hyperparameter settings that will define custom learners.
#' @param detailed_names Set to T to have the function names include the
#'   parameter configurations.
#' @param env Environment in which to create the functions. Defaults
#'   to the current environment (e.g. often the global environment).
#' @param name_prefix The prefix string for the name of each function that is
#'   generated.
#' @param verbose Display extra details.
#'
#' @return Returns a list with expanded tuneGrid and the names of the created
#'   functions.
#'
#' @examples
#' \dontrun{
#' # Create a randomForest learner with ntree set to 1000 rather than the
#' # default of 500.
#' create_rf = create.Learner("SL.randomForest", list(ntree = 1000))
#' create_rf
#' sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial())
#' sl
#' # Clean up global environment.
#' rm(list = create_rf$names)

#' # Create a randomForest learner that optimizes over mtry
#' create_rf = create.Learner("SL.randomForest",
#'                      tune = list(mtry = round(c(1, sqrt(ncol(X)), ncol(X)))))
#' create_rf
#' sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial())
#' sl
#' # Clean up global environment.
#' rm(list = create_rf$names)
#'
#' # Optimize elastic net over alpha, with a custom environment and detailed names.
#' learners = new.env()
#' create_enet = create.Learner("SL.glmnet", env = learners, detailed_names = T,
#'                            tune = list(alpha = seq(0, 1, length.out=5)))
#' create_enet
#' # List the environment to review what functions were created.
#' ls(learners)
#' # We can simply list the environment to specify the library.
#' sl = SuperLearner(Y = Y, X = X, SL.library = ls(learners), family = binomial(), env = learners)
#' sl
#' }
#'
#' @export
create.Learner = function(base_learner, params = list(), tune = list(),
                                   env = parent.frame(), name_prefix = base_learner,
                                   detailed_names = F, verbose = F) {
  if (length(tune) > 0) {
    tuneGrid = expand.grid(tune, stringsAsFactors = FALSE)
    names = rep("", nrow(tuneGrid))
    max_runs = nrow(tuneGrid)
  } else {
    # Run once if no tuneGrid is defined, otherwise run once per grid row.
    max_runs = 1
    tuneGrid = NULL
    names = c()
  }

  for (i in seq(max_runs)) {

    name = paste(name_prefix, i, sep="_")

    if (length(tune) > 0) {
      # Specify drop=F in case tuneGrid is a single-column dataframe.
      g = tuneGrid[i, , drop=F]

      # Separate with "_" because some hyperparameters could be floats with a period.
      if (detailed_names) {
        name = do.call(paste, c(list(name_prefix), g, list(sep="_")))
      }
    } else {
      g = c()
    }

    names[i] = name

    # Create the custom learner function. This approach allows us to not specify
    # some of the learner arguments. and have the function use its own defaults.
    # Or we can set those arguments to "NULL".
    fn_params = ""
    all_params = c(as.list(g), params)
    for (name_i in names(all_params)) {
      val = all_params[[name_i]]
      # Ignore a parameter if it's set to a real NULL or the string "NULL".
      # May need to tweak this if someone really needs to pass a NULL for some reason.
      if (!is.null(val) && val != "NULL") {
        # Add quotes around val if it is a string rather than numeric.
        if (class(val) == "character") {
          val = paste0('"', val, '"')
        }
        fn_params = paste0(fn_params, ", ", name_i, "=", val)
      }
    }

    fn = paste0(name, " <- function(...) ", base_learner, "(...", fn_params, ")")
    if (verbose) {
      cat(fn, "\n")
    }
    eval(parse(text = fn), envir = env)
  }
  results = list(grid = tuneGrid, names = names, base_learner = base_learner,
                 params = params)
  invisible(results)
}