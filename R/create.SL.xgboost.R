#' Factory for XGBoost SL wrappers
#'
#' Create multiple configurations of XGBoost learners based on the desired combinations of hyperparameters.
#'
#' @param tune List of hyperparameter settings to test. If specified, each hyperparameter will need to be defined.
#' @param detailed_names Set to T to have the function names include the parameter configurations.
#' @param env Environment in which to create the SL.xgboost functions. Defaults to the global environment.
#' @param name_prefix The prefix string for the name of each function that is generated.
#'
#' @seealso [SL.xgboost()]
#'
#' @examples
#'
#' # Create a new environment to store the learner functions.
#' # This keeps the global environment organized.
#' sl_env = new.env()
#' # Create 2 * 2 * 1 * 3 = 12 combinations of hyperparameters.
#' tune = list(ntrees = c(100, 500), max_depth = c(1, 2), minobspernode = 10,
#'             shrinkage = c(0.1, 0.01, 0.001))
#' # Generate a separate learner for each combination.
#' xgb_grid = create.SL.xgboost(tune = tune, env = sl_env)
#' # Review the function configurations.
#' xgb_grid
#' # Attach the environment so that the custom learner functions can be accessed.
#' attach(sl_env)
#' \dontrun{
#' sl = SuperLearner(Y = Y, X = X, SL.library = xgb_grid$names)
#' }
#' detach(sl_env)
#'
#' @export
create.SL.xgboost <- function(tune = list(ntrees = c(1000), max_depth = c(4), shrinkage = c(0.1),
                                          minobspernode = c(10)),
                              detailed_names = FALSE,
                              env = .GlobalEnv,
                              name_prefix = "SL.xgb") {
  # Create all combinations of hyperparameters, for grid-like search.
  tuneGrid <- expand.grid(tune, stringsAsFactors = FALSE)

  slnames <- character(nrow(tuneGrid))

  for (i in seq_len(nrow(tuneGrid))) {
    g <- tuneGrid[i,]

    if (detailed_names) {
      slnames[i] <- paste(name_prefix, g$ntrees, g$max_depth, g$shrinkage, g$minobspernode, sep = ".")
    }
    else {
      slnames[i] <- paste(name_prefix, i, sep = ".")
    }

    text <- sprintf("
      %s <- function(..., ntrees = %s, max_depth = %s, shrinkage = %s, minobspernode = %s) {
         SL.xgboost(..., ntrees = ntrees, max_depth = max_depth, shrinkage = shrinkage, minobspernode = minobspernode)
      }
    ", slnames[i], g$ntrees, g$max_depth, g$shrinkage, g$minobspernode)

    exp <- str2expression(text)

    eval(exp, envir = env)
  }

  results <- list(grid = tuneGrid, names = slnames)

  invisible(results)
}