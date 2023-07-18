#' XGBoost SuperLearner wrapper
#'
#' Supports the Extreme Gradient Boosting package for SuperLearnering, which is
#' a variant of gradient boosted machines (GBM).
#'
#' The performance of XGBoost, like GBM, is sensitive to the configuration
#' settings. Therefore it is best to create multiple configurations using
#' create.SL.xgboost and allow the SuperLearner to choose the best weights based
#' on cross-validated performance.
#'
#' If you run into errors please first try installing the latest version of
#' XGBoost from drat as described here:
#' \url{http://xgboost.readthedocs.io/en/latest/build.html}
#'
#' @param Y Outcome variable
#' @param X Covariate dataframe
#' @param newX Optional dataframe to predict the outcome
#' @param obsWeights Optional observation-level weights (supported but not tested)
#' @param id Optional id to group observations from the same unit (not used
#'   currently).
#' @param family "gaussian" for regression, "binomial" for binary
#'   classification, "multinomial" for multiple classification (not yet supported).
#' @param ntrees How many trees to fit. Low numbers may underfit but high
#'   numbers may overfit, depending also on the shrinkage.
#' @param max_depth How deep each tree can be. 1 means no interactions, aka tree
#'   stubs.
#' @param shrinkage How much to shrink the predictions, in order to reduce
#'   overfitting.
#' @param minobspernode Minimum observations allowed per tree node, after which
#'   no more splitting will occur.
#' @param params Many other parameters can be customized. See
#'   \url{http://xgboost.readthedocs.io/en/latest/parameter.html}
#' @param nthread How many threads (cores) should xgboost use. Generally we want
#'   to keep this to 1 so that XGBoost does not compete with SuperLearner
#'   parallelization.
#' @param save_period How often (in tree iterations) to save current model to
#'   disk during processing. If NULL does not save model, and if 0 saves model
#'   at the end.
#' @param verbose Verbosity of XGB fitting.
#' @param ... Any remaining arguments (not supported though).
#'
#' @export
SL.xgboost = function(Y, X, newX, family, obsWeights, id, ntrees = 1000,
                      max_depth = 4, shrinkage = 0.1, minobspernode = 10,
                      params = list(),
                      nthread = 1,
                      verbose = 0,
                      save_period = NULL,
                      ...) {
  .SL.require("xgboost")
  if(packageVersion("xgboost") < "0.6") stop("SL.xgboost requires xgboost version >= 0.6, try help(\'SL.xgboost\') for details")
  # X needs to be converted to a matrix first, then an xgb.DMatrix.
  if (!is.matrix(X)) {
    X = model.matrix(~ . - 1, X)
  }

  # Convert to an xgboost compatible data matrix, using the sample weights.
  xgmat = xgboost::xgb.DMatrix(data = X, label = Y, weight = obsWeights)

  # TODO: support early stopping, which requires a "watchlist". See ?xgb.train

  if (family$family == "gaussian") {
    # reg:linear was deprecated in version 1.1.1.1, changed to reg:squarederror
    if(packageVersion("xgboost") >= "1.1.1.1") {
      objective <- 'reg:squarederror'
      } else {
      objective <- 'reg:linear'
    }
    model = xgboost::xgboost(data = xgmat, objective=objective, nrounds = ntrees,
                max_depth = max_depth, min_child_weight = minobspernode, eta = shrinkage,
                verbose = verbose, nthread = nthread, params = params,
                save_period = save_period)
  }
  if (family$family == "binomial") {
    model = xgboost::xgboost(data = xgmat, objective="binary:logistic", nrounds = ntrees,
                max_depth = max_depth, min_child_weight = minobspernode, eta = shrinkage,
                verbose = verbose, nthread = nthread, params = params,
                save_period = save_period, eval_metric = "logloss")
  }
  if (family$family == "multinomial") {
    # TODO: test this.
    model = xgboost::xgboost(data = xgmat, objective="multi:softmax", nrounds = ntrees,
                max_depth = max_depth, min_child_weight = minobspernode, eta = shrinkage,
                verbose = verbose, num_class = length(unique(Y)), nthread = nthread,
                params = params,
                save_period = save_period)
  }

  # Newdata needs to be converted to a matrix first, then an xgb.DMatrix.
  if (!is.matrix(newX)) {
    newX = model.matrix(~ . - 1, newX)
  }

  pred = predict(model, newdata = newX)

  fit = list(object = model)
  class(fit) = c("SL.xgboost")
  out = list(pred = pred, fit = fit)
  return(out)
}

#' XGBoost prediction on new data
#' @param object Model fit object from SuperLearner
#' @param newdata Dataframe that will be converted to an xgb.DMatrix
#' @param family Binomial or gaussian
#' @param ... Any remaining arguments (not supported though).
predict.SL.xgboost <- function(object, newdata, family, ...) {
  .SL.require("xgboost")
  if(packageVersion("xgboost") < "0.6") stop("SL.xgboost requires xgboost version >= 0.6, try help(\'SL.xgboost\') for details")
  # newdata needs to be converted to a matrix first
  if (!is.matrix(newdata)) {
    newdata = model.matrix(~ . - 1, newdata)
  }
  pred = predict(object$object, newdata = newdata)
  return(pred)
}

#' Factory for XGBoost SL wrappers
#'
#' Create multiple configurations of XGBoost learners based on the desired combinations of hyperparameters.
#'
#' @param tune List of hyperparameter settings to test. If specified, each hyperparameter will need to be defined.
#' @param detailed_names Set to T to have the function names include the parameter configurations.
#' @param env Environment in which to create the SL.xgboost functions. Defaults to the global environment.
#' @param name_prefix The prefix string for the name of each function that is generated.
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
#' @export
create.SL.xgboost = function(tune = list(ntrees = c(1000), max_depth = c(4), shrinkage = c(0.1),
                             minobspernode = c(10)), detailed_names = F, env = .GlobalEnv,
                             name_prefix = "SL.xgb") {
  # Create all combinations of hyperparameters, for grid-like search.
  tuneGrid = expand.grid(tune, stringsAsFactors=F)

  names = rep("", nrow(tuneGrid))

  for (i in seq(nrow(tuneGrid))) {
    g = tuneGrid[i,]
    if (detailed_names) {
      name = paste(name_prefix, g$ntrees, g$max_depth, g$shrinkage, g$minobspernode, sep=".")
    } else {
      name = paste(name_prefix, i, sep=".")
    }
    names[i] = name
    eval(parse(text = paste0(name, "= function(..., ntrees = ", g$ntrees, ", max_depth = ", g$max_depth, ", shrinkage=", g$shrinkage, ", minobspernode=", g$minobspernode, ") SL.xgboost(..., ntrees = ntrees, max_depth = max_depth, shrinkage=shrinkage, minobspernode=minobspernode)")), envir = env)
  }
  results = list(grid = tuneGrid, names = names)
  invisible(results)
}
