#' SL wrapper for XGBoost
#'
#' Supports the Extreme Gradient Boosting package for SuperLearning, which is
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
#' @inheritParams SL.template
#' @inheritParams predict.SL.template
#' @inheritParams SL.glm
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
#' @seealso [create.SL.xgboost()] to create new xgboost wrappers with different parameters.

#' @export
SL.xgboost = function(Y, X, newX = X, family = gaussian(), obsWeights = NULL, ntrees = 1000,
                      max_depth = 4, shrinkage = 0.1, minobspernode = 10,
                      params = list(),
                      nthread = 1,
                      verbose = 0,
                      save_period = NULL,
                      ...) {
  .SL.require("xgboost")

  if (utils::packageVersion("xgboost") < "0.6") {
    stop("SL.xgboost requires xgboost version >= 0.6, try help(\'SL.xgboost\') for details")
  }

  # Convert to an xgboost compatible data matrix, using the sample weights.
  xgmat <- xgboost::xgb.DMatrix(data = X, label = Y, weight = obsWeights)

  # TODO: support early stopping, which requires a "watchlist". See ?xgb.train

  if (family$family == "gaussian") {
    xgbpar <- xgboost::xgb.params(objective = "reg:squarederror",
                                  nthread = nthread,
                                  eta = shrinkage,
                                  max_depth = max_depth,
                                  min_child_weight = minobspernode)
  }
  else if (family$family == "binomial") {
    xgbpar <- xgboost::xgb.params(objective = "binary:logistic",
                                  nthread = nthread,
                                  eta = shrinkage,
                                  max_depth = max_depth,
                                  min_child_weight = minobspernode,
                                  eval_metric = "logloss")
  }
  else if (family$family == "multinomial") {
    # TODO: test this.
    xgbpar <- xgboost::xgb.params(objective = "multi:softmax",
                                  nthread = nthread,
                                  eta = shrinkage,
                                  max_depth = max_depth,
                                  min_child_weight = minobspernode,
                                  num_class = length(unique(Y)))
  }

  for (i in intersect(names(params), names(formals(xgboost::xgb.params)))) {
    xgbpar[[i]] <- params[[i]]
  }

  model <- xgboost::xgb.train(params = xgbpar, data = xgmat, nrounds = ntrees,
                              save_period = save_period, verbose = verbose)

  pred <- predict(model, newdata = xgboost::xgb.DMatrix(data = newX))

  fit <- list(object = model)
  class(fit) <- c("SL.xgboost")

  list(pred = pred, fit = fit)
}

#' @exportS3Method predict SL.xgboost
#' @rdname SL.xgboost
predict.SL.xgboost <- function(object, newdata, ...) {
  .SL.require("xgboost")

  if (utils::packageVersion("xgboost") < "0.6") {
    stop("SL.xgboost requires xgboost version >= 0.6, try help(\'SL.xgboost\') for details")
  }

  predict(object$object, newdata = xgboost::xgb.DMatrix(data = newdata))
}
