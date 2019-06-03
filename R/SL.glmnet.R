#' @title Elastic net regression, including lasso and ridge
#'
#' @description
#' Penalized regression using elastic net. Alpha = 0 corresponds to ridge
#' regression and alpha = 1 corresponds to Lasso.
#'
#' See \code{vignette("glmnet_beta", package = "glmnet")} for a nice tutorial on
#' glmnet.
#'
#' @param Y Outcome variable
#' @param X Covariate dataframe
#' @param newX Dataframe to predict the outcome
#' @param obsWeights Optional observation-level weights
#' @param id Optional id to group observations from the same unit (not used
#'   currently).
#' @param family "gaussian" for regression, "binomial" for binary
#'   classification. Untested options: "multinomial" for multiple classification
#'   or "mgaussian" for multiple response, "poisson" for non-negative outcome
#'   with proportional mean and variance, "cox".
#' @param alpha Elastic net mixing parameter, range [0, 1]. 0 = ridge regression
#'   and 1 = lasso.
#' @param nfolds Number of folds for internal cross-validation to optimize lambda.
#' @param nlambda Number of lambda values to check, recommended to be 100 or more.
#' @param loss Loss function, can be "deviance", "mse", or "mae". If family =
#'   binomial can also be "auc" or "class" (misclassification error).
#' @param useMin If TRUE use lambda that minimizes risk, otherwise use 1
#'   standard-error rule which chooses a higher penalty with performance within
#'   one standard error of the minimum (see Breiman et al. 1984 on CART for
#'   background).
#' @param ... Any additional arguments are passed through to cv.glmnet.
#'
#' @examples
#'
#' # Load a test dataset.
#' data(PimaIndiansDiabetes2, package = "mlbench")
#' data = PimaIndiansDiabetes2
#'
#' # Omit observations with missing data.
#' data = na.omit(data)
#'
#' Y = as.numeric(data$diabetes == "pos")
#' X = subset(data, select = -diabetes)
#'
#' set.seed(1, "L'Ecuyer-CMRG")
#'
#' sl = SuperLearner(Y, X, family = binomial(),
#'                   SL.library = c("SL.mean", "SL.glm", "SL.glmnet"))
#' sl
#'
#' @references
#'
#' Friedman, J., Hastie, T., & Tibshirani, R. (2010). Regularization paths for
#' generalized linear models via coordinate descent. Journal of statistical
#' software, 33(1), 1.
#'
#' Hoerl, A. E., & Kennard, R. W. (1970). Ridge regression: Biased estimation
#' for nonorthogonal problems. Technometrics, 12(1), 55-67.
#'
#' Tibshirani, R. (1996). Regression shrinkage and selection via the lasso.
#' Journal of the Royal Statistical Society. Series B (Methodological), 267-288.
#'
#' Zou, H., & Hastie, T. (2005). Regularization and variable selection via the
#' elastic net. Journal of the Royal Statistical Society: Series B (Statistical
#' Methodology), 67(2), 301-320.
#'
#' @seealso \code{\link{predict.SL.glmnet}} \code{\link[glmnet]{cv.glmnet}}
#'   \code{\link[glmnet]{glmnet}}
#'
#' @export
SL.glmnet <- function(Y, X, newX, family, obsWeights, id,
                      alpha = 1, nfolds = 10, nlambda = 100, useMin = TRUE,
                      loss = "deviance",
                      ...) {
  .SL.require('glmnet')

  # X must be a matrix, should we use model.matrix or as.matrix
  # TODO: support sparse matrices.
  if (!is.matrix(X)) {
    X <- model.matrix(~ -1 + ., X)
    newX <- model.matrix(~ -1 + ., newX)
  }

  # Use CV to find optimal lambda.
  fitCV <- glmnet::cv.glmnet(x = X, y = Y, weights = obsWeights,
                             lambda = NULL,
                             type.measure = loss,
                             nfolds = nfolds,
                             family = family$family,
                             alpha = alpha,
                             nlambda = nlambda,
                             ...)

  # If we predict with the cv.glmnet object we can specify lambda using a
  # string.
  pred <- predict(fitCV, newx = newX, type = "response",
                  s = ifelse(useMin, "lambda.min", "lambda.1se"))

  fit <- list(object = fitCV, useMin = useMin)
  class(fit) <- "SL.glmnet"

  out <- list(pred = pred, fit = fit)
  return(out)
}

#' @title Prediction for an SL.glmnet object
#'
#' @description Prediction for the glmnet wrapper.
#'
#' @param object Result object from SL.glmnet
#' @param newdata Dataframe or matrix that will generate predictions.
#' @param remove_extra_cols Remove any extra columns in the new data that were
#'   not part of the original model.
#' @param add_missing_cols Add any columns from original data that do not exist
#'   in the new data, and set values to 0.
#' @param ... Any additional arguments (not used).
#'
#' @seealso \code{\link{SL.glmnet}}
#'
#' @export
predict.SL.glmnet <- function(object, newdata,
                              remove_extra_cols = T,
                              add_missing_cols = T,
                              ...) {
  .SL.require('glmnet')

  # TODO: support sparse matrices.
  if (!is.matrix(newdata)) {
    newdata <- model.matrix(~ -1 + ., newdata)
  }

  original_cols = rownames(object$object$glmnet.fit$beta)

  # Remove any columns in newdata that were not present in original data.
  if (remove_extra_cols) {
    extra_cols = setdiff(colnames(newdata), original_cols)
    if (length(extra_cols) > 0) {
      warning(paste("Removing extra columns in prediction data:",
                     paste(extra_cols, collapse = ", ")))

      newdata = newdata[, !colnames(newdata) %in% extra_cols, drop = FALSE]
    }
  }

  # Add any columns in original data that are not present in new data.
  if (add_missing_cols) {
    missing_cols = setdiff(original_cols, colnames(newdata))
    if (length(missing_cols) > 0) {
      warning(paste("Adding missing columns in prediction data:",
                     paste(missing_cols, collapse = ", ")))

      new_cols = matrix(0, nrow = nrow(newdata), ncol = length(missing_cols))
      colnames(new_cols) = missing_cols
      newdata = cbind(newdata, new_cols)

      # Sort columns in the correct order so that matrix multiplication is correct.
      newdata = newdata[, original_cols, drop = FALSE]
    }
  }

  # If we predict with the cv.glmnet object we can specify lambda using a
  # string.
  pred <- predict(object$object, newx = newdata, type = "response",
             s = ifelse(object$useMin, "lambda.min", "lambda.1se"))

  return(pred)
}
