#' Factory for RandomForest SL wrappers
#'
#' Create multiple configurations of RandomForest learners based on the desired
#' combinations of hyperparameters.
#'
#' @param tune List of hyperparameter settings to test. If specified, each
#'   hyperparameter will need to be defined.
#' @param detailed_names Set to T to have the function names include the
#'   parameter configurations.
#' @param env Environment in which to create the SL.xgboost functions. Defaults
#'   to the global environment.
#' @param name_prefix The prefix string for the name of each function that is
#'   generated.
#' @param verbose Display extra details.
#'
#' @return Returns a list with expanded tuneGrid and the names of the created functions.
#'
#' @examples
#'
#' # Create the functions in the global environment.
#' create_rf = create.SL.randomForest()
#' create_rf
#' sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial())
#' sl
#' # Clean up global environment.
#' do.call(rm, as.list(create_rf$names))
#'
#' # Customize the mtry parameter.
#' tune_rf = list(mtry = c(4, 8))
#' create_rf = create.SL.randomForest(tune = tune_rf, detailed_names = T)
#' sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial())
#'
#' # Create the learners in a custom environment rather than .GlobalEnv.
#' sl_env = new.env()
#' create_rf = create.SL.randomForest(env = sl_env)
#' create_rf
#' # Attach the environment with the learner functions so SL can access them.
#' attach(sl_env)
#' sl = SuperLearner(Y = Y, X = X, SL.library = create_rf$names, family = binomial())
#' sl
#' detach(sl_env)
#'
#' @export
create.SL.randomForest <- function(tune = list(mtry = c(1, 5, 10), nodesize = c(1, 5, 10),
                                               maxnodes = "NULL"),
                                   env = .GlobalEnv, name_prefix = "SL.randomForest",
                                   detailed_names = F, verbose=F) {
  tuneGrid <- expand.grid(tune, stringsAsFactors = FALSE)

  names = rep("", nrow(tuneGrid))

  for (i in seq(nrow(tuneGrid))) {
    # Specify drop=F in case tuneGrid is a single-column dataframe.
    g = tuneGrid[i, , drop=F]

    # Separate with "_" because some hyperparameters could be floats with a period.
    if (detailed_names) {
      name = paste(name_prefix, g$mtry, g$nodesize, g$maxnodes, sep="_")
    } else {
      name = paste(name_prefix, i, sep="_")
    }
    names[i] = name

    # Create the custom learner function.
    # This approach allows us to not specify some of the learner arguments
    # and have the function use its own defaults. Or we can set those arguments to "NULL".
    fn = paste0(name, " <- function(...) SL.randomForest(...",
            ifelse(!is.null(g$mtry) && g$mtry != "NULL", paste0(", mtry = ", g$mtry), ""),
            ifelse(!is.null(g$nodesize) && g$nodesize != "NULL", paste0(", nodesize = ", g$nodesize), ""),
            ifelse(!is.null(g$maxnodes) && g$maxnodes != "NULL", paste0(", maxnodes = ", g$maxnodes), ""),
         ")")
    if (verbose) {
      cat(fn, "\n")
    }
    eval(parse(text = fn), envir = env)
  }
  results = list(grid = tuneGrid, names = names)
  invisible(results)
}