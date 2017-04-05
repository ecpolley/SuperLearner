#' @title SL wrapper for ranger
#' @description Ranger is a fast implementation of Random Forest (Breiman 2001)
#'   or recursive partitioning, particularly suited for high dimensional data.
#'
#' Extending code by Eric Polley from the SuperLearnerExtra package.
#'
#' @param Y Outcome variable
#' @param X Training dataframe
#' @param newX Test dataframe
#' @param family Gaussian or binomial
#' @param obsWeights Observation-level weights
#' @param num.trees Number of trees.
#' @param mtry Number of variables to possibly split at in each node. Default is
#'   the (rounded down) square root of the number variables.
#' @param write.forest Save ranger.forest object, required for prediction. Set
#'   to FALSE to reduce memory usage if no prediction intended.
#' @param probability Grow a probability forest as in Malley et al. (2012).
#' @param min.node.size Minimal node size. Default 1 for classification, 5 for
#'   regression, 3 for survival, and 10 for probability.
#' @param replace Sample with replacement.
#' @param sample.fraction Fraction of observations to sample. Default is 1 for
#'   sampling with replacement and 0.632 for sampling without replacement.
#' @param num.threads Number of threads to use.
#' @param verbose If TRUE, display additional output during execution.
#' @param ... Any additional arguments, not currently used.
#'
#' @examples
#'
#' data(Boston, package = "MASS")
#' Y = Boston$medv
#' # Remove outcome from covariate dataframe.
#' X = Boston[, -14]
#'
#' set.seed(1)
#'
#' # Use only 2 CV folds to speed up example.
#' sl = SuperLearner(Y, X, family = gaussian(), cvControl = list(V = 2),
#'                  SL.library = c("SL.mean", "SL.ranger"))
#' sl
#'
#' pred = predict(sl, X)
#' summary(pred$pred)
#'
#' @references
#'
#' Breiman, L. (2001). Random forests. Machine learning 45:5-32.
#'
#' Wright, M. N. & Ziegler, A. (2016). ranger: A Fast Implementation of Random
#' Forests for High Dimensional Data in C++ and R. Journal of Statistical
#' Software, in press. http://arxiv.org/abs/1508.04409.
#'
#' @seealso \code{\link{SL.ranger}} \code{\link[ranger]{ranger}}
#'   \code{\link[ranger]{predict.ranger}}
#'
#' @export
SL.ranger <-
  function(Y, X, newX, family,
           obsWeights,
           num.trees = 500,
           mtry = floor(sqrt(ncol(X))),
           write.forest = TRUE,
           probability = family$family == "binomial",
           min.node.size = ifelse(family$family == "gaussian", 5, 1),
           replace = TRUE,
           sample.fraction = ifelse(replace, 1, 0.632),
           num.threads = 1,
           verbose = T,
           ...) {
  # need write.forest = TRUE for predict method
  .SL.require("ranger")

  if (family$family == "binomial") {
    Y = as.factor(Y)
  }

  # Ranger does not seem to work with X as a matrix, so we explicitly convert to
  # data.frame rather than cbind. newX can remain as-is though.
  if (is.matrix(X)) {
    X = data.frame(X)
  }

  # Use _Y as our outcome variable name to avoid a possible conflict with a
  # variable in X named "Y".
  fit <- ranger::ranger(`_Y` ~ ., data = cbind("_Y" = Y, X),
                        num.trees = num.trees,
                        mtry = mtry,
                        min.node.size = min.node.size,
                        replace = replace,
                        sample.fraction = sample.fraction,
                        case.weights = obsWeights,
                        write.forest = write.forest,
                        probability = probability,
                        num.threads = num.threads,
                        verbose = verbose)

  pred <- predict(fit, data = newX)$predictions

  # For binomial family $predictions is a two-column matrix.
  if (family$family == "binomial") {
    # P(Y = 1 | X) for binomial.
    pred = pred[, "1"]
  }

  fit <- list(object = fit, verbose = verbose)
  class(fit) <- c("SL.ranger")
  out <- list(pred = pred, fit = fit)
  return(out)
}

#' @title Prediction wrapper for ranger random forests
#'
#' @description Prediction wrapper for SL.ranger objects.
#'
#' @param object SL.kernlab object
#' @param newdata Dataframe to generate predictions
#' @param family Gaussian or binomial
#' @param num.threads Number of threads used for parallelization
#' @param verbose If TRUE output additional information during execution.
#' @param ... Unused additional arguments
#'
#' @seealso \code{\link{SL.ranger}} \code{\link[ranger]{ranger}}
#'   \code{\link[ranger]{predict.ranger}}
#'
#' @export
predict.SL.ranger <- function(object, newdata, family,
                              num.threads = 1,
                              verbose = object$verbose,
                              ...) {
  .SL.require("ranger")

  # Binomial and gaussian prediction is the same.
  pred <- predict(object$object, data = newdata, verbose = verbose,
                  num.threads = num.threads)$predictions

  # For binomial family $predictions is a two-column matrix.
  if (family$family == "binomial") {
    # P(Y = 1 | X) for binomial.
    pred = pred[, "1"]
  }

  pred
}
