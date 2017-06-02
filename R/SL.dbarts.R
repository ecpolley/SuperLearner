#' Discrete bayesian additive regression tree sampler
#'
#' BART algorithm implemented in C++, but without predict() support.
#'
#' @param Y Outcome variable
#' @param X Covariate dataframe
#' @param newX Optional dataframe to predict the outcome. dbarts does not
#'   support predict() so any prediction needs to be via newX passed during
#'   model training.
#' @param obsWeights Optional observation-level weights.
#' @param id Optional id to group observations from the same unit (not used
#'   currently).
#' @param family "gaussian" for regression, "binomial" for binary
#'   classification.
#' @param sigest For continuous response models, an estimate of the error
#'   variance, \eqn{\sigma^2}, used to calibrate an inverse-chi-squared prior used on that
#'   parameter. If not supplied, the least-squares estimate is derived instead.
#'   See sigquant for more information. Not applicable when y is binary.
#' @param sigdf Degrees of freedom for error variance prior. Not applicable when
#'   y is binary.
#' @param sigquant The quantile of the error variance prior that the rough
#'   estimate (sigest) is placed at. The closer the quantile is to 1, the more
#'   aggresive the fit will be as you are putting more prior weight on error
#'   standard deviations (\eqn{\sigma}) less than the rough estimate. Not applicable when y
#'   is binary.
#' @param k For numeric y, k is the number of prior standard deviations E(Y|x) =
#'   f(x) is away from +/- 0.5. The response (Y) is internally scaled to range
#'   from -0.5 to 0.5. For binary y, k is the number of prior standard
#'   deviations f(x) is away from +/- 3. In both cases, the bigger k is, the
#'   more conservative the fitting will be.
#' @param power Power parameter for tree prior.
#' @param base Base parameter for tree prior.
#' @param binaryOffset Used for binary y. When present, the model is P(Y = 1 |
#'   x) = \eqn{\Phi}(f(x) + binaryOffset), allowing fits with probabilities shrunk
#'   towards values other than 0.5.
#' @param ntree The number of trees in the sum-of-trees formulation.
#' @param ndpost The number of posterior draws after burn in, ndpost / keepevery
#'   will actually be returned.
#' @param nskip Number of MCMC iterations to be treated as burn in.
#' @param printevery As the MCMC runs, a message is printed every printevery
#'   draws.
#' @param keepevery Every keepevery draw is kept to be returned to the user.
#'   Useful for "thinning" samples.
#' @param keeptrainfits If TRUE the draws of f(x) for x corresponding to the
#'   rows of x.train are returned.
#' @param usequants When TRUE, determine tree decision rules using estimated
#'   quantiles derived from the x.train variables. When FALSE, splits are
#'   determined using values equally spaced across the range of a variable. See
#'   details for more information.
#' @param numcut The maximum number of possible values used in decision rules
#'   (see usequants, details). If a single number, it is recycled for all
#'   variables; otherwise must be a vector of length equal to ncol(x.train).
#'   Fewer rules may be used if a covariate lacks enough unique values.
#' @param printcutoffs The number of cutoff rules to printed to screen before
#'   the MCMC is run. Given a single integer, the same value will be used for
#'   all variables. If 0, nothing is printed.
#' @param nthread Integer specifying how many threads to use for rudimentary
#'   calculations such as means/variances. Depending on the CPU architecture,
#'   using more than one can degrade performance for small/medium data sets. As
#'   such some calculations may be executed single threaded regardless.
#' @param keepcall Logical; if FALSE, returned object will have call set to
#'   call("NULL"), otherwise the call used to instantiate BART.
#' @param verbose If T output additional information during training.
#' @param ... Any remaining arguments (unused)
#'
#' @references
#'
#' Chipman, H. A., George, E. I., & McCulloch, R. E. (2010). BART: Bayesian
#' additive regression trees. The Annals of Applied Statistics, 4(1), 266-298.
#' doi: 10.1214/09-AOAS285 (URL: http://doi.org/10.1214/09-AOAS285).
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
#' # Sample rows to speed up example.
#' row_subset = sample(nrow(X), 30)
#'
#' sl = SuperLearner(Y[row_subset], X[row_subset, ], family = gaussian(),
#' cvControl = list(V = 2), SL.library = c("SL.mean", "SL.dbarts"))
#'
#' print(sl)
#'
#' @encoding utf-8
#' @export
SL.dbarts = function(Y, X, newX, family, obsWeights, id,
                     sigest = NA,
                     sigdf = 3,
                     sigquant = 0.90,
                     k = 2.0,
                     power = 2.0,
                     base = 0.95,
                     binaryOffset = 0.0,
                     ntree = 200,
                     ndpost = 1000,
                     nskip = 100,
                     printevery = 100,
                     keepevery = 1,
                     keeptrainfits = T,
                     usequants = F,
                     numcut = 100,
                     printcutoffs = 0,
                     nthread = 1,
                     keepcall = T,
                     verbose = F,
                     ...) {

  .SL.require("dbarts")

  model =
    dbarts::bart(x.train = X,
                 y.train = Y,
                 # We need to pass newX in directly due to lack of prediction.
                 x.test = newX,
                 sigest = sigest,
                 sigdf = sigdf,
                 sigquant = sigquant,
                 k = k,
                 power = power,
                 base = base,
                 binaryOffset = binaryOffset,
                 weights = obsWeights,
                 ntree = ntree,
                 ndpost = ndpost,
                 nskip = nskip,
                 printevery = printevery,
                 keepevery = keepevery,
                 keeptrainfits = keeptrainfits,
                 usequants = usequants,
                 numcut = numcut,
                 printcutoffs = printcutoffs,
                 nthread = nthread,
                 keepcall = keepcall,
                 verbose = verbose)

  # TODO: there is no predict!
  #pred = predict(model, newdata = newX)
  if (family$family == "gaussian") {
    pred = model$yhat.test.mean
  } else {
    # No mean is provided for binary Y :/
    pred = colMeans(model$yhat.test)
  }

  fit = list(object = model)
  class(fit) = c("SL.dbarts")
  out = list(pred = pred, fit = fit)
  return(out)
}

#' dbarts prediction
#'
#' WARNING: dbarts does not currently support predict(). Must use newX when
#' training via SL.dbarts.
#'
#' @param object SuperLearner object
#' @param newdata Dataframe to predict the outcome
#' @param family "gaussian" for regression, "binomial" for binary
#'   classification. (Not used)
#' @param ... Additional arguments (not used)
#'
#' @export
predict.SL.dbarts = function(object, newdata, family, ...) {

  .SL.require("dbarts")

  # TODO: have to refit data, or just stop().

  #pred = predict(object$object, newdata = newdata)
  stop(paste0("dbarts does not support predict() currently.",
              "Pass in any prediction data to SuperLearner() as newX"))

  #return(pred)
}

