#' Wrapper for bartMachine learner
#'
#' Support Bayesian additive regression trees via the \pkg{bartMachine} package.
#'
#' @inheritParams SL.template
#' @inheritParams predict.SL.template
#' @param obsWeights Optional observation-level weights (supported but not tested)
#' @param id Optional id to group observations from the same unit (not used
#'   currently).
#' @param num_trees The number of trees to be grown in the sum-of-trees model.
#' @param num_burn_in Number of MCMC samples to be discarded as "burn-in".
#' @param num_iterations_after_burn_in Number of MCMC samples to draw from the
#'   posterior distribution of \eqn{f(x)}.
#' @param alpha Base hyperparameter in tree prior for whether a node is
#'   nonterminal or not.
#' @param beta Power hyperparameter in tree prior for whether a node is
#'   nonterminal or not.
#' @param k For regression, `k` determines the prior probability that \eqn{E(Y|X)} is
#'   contained in the interval \eqn{[y_\text{min}, y_\text{max}]}, based on a normal
#'   distribution. For example, when `k = 2`, the prior probability is 95%. For
#'   classification, `k` determines the prior probability that \eqn{E(Y|X)} is between
#'   -3 and 3. Note that a larger value of `k` results in more shrinkage and a more
#'   conservative fit.
#' @param q Quantile of the prior on the error variance at which the data-based
#'   estimate is placed. Note that the larger the value of q, the more
#'   aggressive the fit as you are placing more prior weight on values lower
#'   than the data-based estimate. Not used for classification.
#' @param nu Degrees of freedom for the inverse \eqn{\chi^2} prior. Not used for
#'   classification.
#' @param verbose Prints information about progress of the algorithm to the
#'   screen.
#' @param ... Additional arguments (not used)

#' @export
SL.bartMachine <- function(Y, X, newX = X, family = gaussian(), obsWeights = NULL, id,
                   num_trees = 50, num_burn_in = 250, verbose = F,
                   alpha = 0.95, beta = 2, k = 2, q = 0.9, nu = 3,
                   num_iterations_after_burn_in = 1000, ...) {
  .SL.require("bartMachine")
  model = bartMachine::bartMachine(X, Y, num_trees = num_trees,
                        num_burn_in = num_burn_in, verbose = verbose,
                        alpha = alpha, beta = beta, k = k, q = q, nu = nu,
                        num_iterations_after_burn_in = num_iterations_after_burn_in)
  # pred returns predicted responses (on the scale of the outcome)
  pred <- predict(model, newX)
  # fit returns all objects needed for predict.SL.template
  fit <- list(object = model)
  #fit <- vector("list", length=0)
  class(fit) <- c("SL.bartMachine")
  out <- list(pred = pred, fit = fit)
  return(out)
}

#' @exportS3Method predict SL.bartMachine
#' @rdname SL.bartMachine
predict.SL.bartMachine <- function(object, newdata, ...) {
  .SL.require("bartMachine")

  predict(object$object, newdata)
}
