#' Recombine a SuperLearner fit using a new metalearning method
#'
#' `recombineSL()` takes an existing SuperLearner fit and a new
#' metalearning method and returns a new SuperLearner fit with updated base
#' learner weights.
#'
#' \code{recombineSL()} re-fits the super learner prediction algorithm using a
#' new metalearning method.  The weights for each algorithm in
#' \code{SL.library} are re-estimated using the new metalearner, however the
#' base learner fits are not regenerated, so this function saves a lot of
#' computation time as opposed to using the \code{SuperLearner()} function with a
#' new \code{method} argument.  The output is identical to the output from the
#' \code{SuperLearner()} function.
#'
#' @inheritParams SuperLearner
#' @param object Fitted object from \code{SuperLearner}.
#'
#' @inherit SuperLearner return
#'
#' @author Erin LeDell \email{ledell@@berkeley.edu}
#'
#' @references
#' van der Laan, M. J., Polley, E. C. and Hubbard, A. E. (2008) Super Learner, \emph{Statistical Applications of Genetics and Molecular Biology}, \bold{6}, article 25.
#'
#' @keywords models
#'
#' @examples
#' \dontrun{
#' # Binary outcome example adapted from SuperLearner examples
#'
#' set.seed(1)
#' N <- 200
#' X <- matrix(rnorm(N*10), N, 10)
#' X <- as.data.frame(X)
#' Y <- rbinom(N, 1, plogis(.2*X[, 1] + .1*X[, 2] - .2*X[, 3] +
#'   .1*X[, 3]*X[, 4] - .2*abs(X[, 4])))
#'
#' SL.library <- c("SL.glmnet", "SL.glm", "SL.knn", "SL.gam", "SL.mean")
#'
#' # least squares loss function
#' set.seed(1) # for reproducibility
#' fit_nnls <- SuperLearner(Y = Y, X = X, SL.library = SL.library,
#'   verbose = TRUE, method = "method.NNLS", family = binomial())
#' fit_nnls
#' #                    Risk       Coef
#' # SL.glmnet_All 0.2439433 0.01293059
#' # SL.glm_All    0.2461245 0.08408060
#' # SL.knn_All    0.2604000 0.09600353
#' # SL.gam_All    0.2471651 0.40761918
#' # SL.mean_All   0.2486049 0.39936611
#'
#'
#' # negative log binomial likelihood loss function
#' fit_nnloglik <- recombineSL(fit_nnls, Y = Y, method = "method.NNloglik")
#' fit_nnloglik
#' #                    Risk      Coef
#' # SL.glmnet_All 0.6815911 0.1577228
#' # SL.glm_All    0.6918926 0.0000000
#' # SL.knn_All          Inf 0.0000000
#' # SL.gam_All    0.6935383 0.6292881
#' # SL.mean_All   0.6904050 0.2129891
#'
#' # If we use the same seed as the original `fit_nnls`, then
#' # the recombineSL and SuperLearner results will be identical
#' # however, the recombineSL version will be much faster since
#' # it doesn't have to re-fit all the base learners.
#' set.seed(1)
#' fit_nnloglik2 <- SuperLearner(Y = Y, X = X, SL.library = SL.library,
#'   verbose = TRUE, method = "method.NNloglik", family = binomial())
#' fit_nnloglik2
#' #                    Risk      Coef
#' # SL.glmnet_All 0.6815911 0.1577228
#' # SL.glm_All    0.6918926 0.0000000
#' # SL.knn_All          Inf 0.0000000
#' # SL.gam_All    0.6935383 0.6292881
#' # SL.mean_All   0.6904050 0.2129891
#'
#' }
#'

# These functions take an existing SuperLearner or CV.SuperLearner fit,
# re-fits the ensemble metalearning step using a new metalearning method,
# specified in the `method` argument and returns the new fit.
# This saves a lot of computation time since we don't have to re-compute the Z
# matrix of cv predicted values by cross-validating each base learner a second time.
# The recombineSL and recombineCVSL functions are stripped down versions of the
# original SuperLearner and CV.SuperLearner functions by Eric C. Polley.


recombineSL <- function(object, Y, method = "method.NNloglik", verbose = FALSE) {

  if (!inherits(object, "SuperLearner")) {
    stop("The supplied 'object' is not of class SuperLearner.")
  }

  if (is.character(method)) {
    if (exists(method, mode = 'list')) {
      method <- get(method, mode = 'list')
    } else if (exists(method, mode = 'function')) {
      method <- get(method, mode = 'function')()
    }
  } else if (is.function(method)) {
    method <- method()
  }
  if (!is.list(method)) {
    stop("method is not in the appropriate format. Check out help('method.template')")
  }
  if (!is.null(method$require)) {
    sapply(method$require, function(x) require(force(x), character.only = TRUE))
  }
  if (identical(object$method, method, ignore.environment=TRUE)) {
    # May want to modify this "if" statement because method.AUC with different args look the same here
    warning("The new method supplied is identical to the existing method.")
  }

  # get relevant objects from the SuperLearner fit object
  obsWeights <- object$obsWeights
  control <- object$control
  cvControl <- object$cvControl
  family <- object$family
  library <- object$SL.library
  libraryNames <- object$libraryNames
  fitLibrary <- object$fitLibrary
  varNames <- object$varNames
  validRows <- object$validRows
  Z <- object$Z
  predY <- object$library.predict
  whichScreen <- object$whichScreen
  errorsInCVLibrary <- object$errorsInCVLibrary
  errorsInLibrary <- object$errorsInLibrary

  # put fitLibrary in it's own environment to locate later
  fitLibEnv <- new.env()
  assign('fitLibrary', fitLibrary, envir = fitLibEnv)
  N <- dim(Z)[1L]
  k <- nrow(library$library)
  if (N != length(Y)) {
    stop("length(Y) does not match nrow(Z). Verify that Y is the same outcome variable that 'object' was trained on.")
  }
  if (is.null(obsWeights)) {
    obsWeights <- rep(1, N)
  }
  if (!is.numeric(Y)) {
    stop("the outcome Y must be a numeric vector")
  }
  # family can be either character or function, so these lines put everything together (code from glm())
  if(is.character(family))
    family <- get(family, mode="function", envir=parent.frame())
  if (is.function(family))
    family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }

  if (family$family != "binomial" & isTRUE("cvAUC" %in% method$require)) {
    stop("'method.AUC' is designed for the 'binomial' family only")
  }

  # Re-compute weights for each algorithm in library using the new metalearner method:
  getCoef <- method$computeCoef(Z = Z, Y = Y, libraryNames = libraryNames,
                                obsWeights = obsWeights, control = control,
                                verbose = verbose)
  coef <- getCoef$coef
  names(coef) <- libraryNames

  # compute super learner predictions on newX
  getPred <- method$computePred(predY = predY, coef = coef, control = control)

  # clean up when errors in library
  if (sum(errorsInCVLibrary) > 0) {
    getCoef$cvRisk[as.logical(errorsInCVLibrary)] <- NA
  }

  object$SL.predict <- getPred
  object$coef = coef
  object$cvRisk = getCoef$cvRisk
  object$family <- family
  object$fitLibrary = get('fitLibrary', envir = fitLibEnv)
  object$method <- method

  object
}

#' Recombine a CV.SuperLearner fit using a new metalearning method
#'
#' Function to re-compute the V-fold cross-validated risk estimate for super
#' learner using a new metalearning method.  This function takes as input an
#' existing CV.SuperLearner fit and applies the \code{recombineSL} fit to each
#' of the V Super Learner fits.
#'
#' The function \code{recombineCVSL} computes the usual V-fold cross-validated
#' risk estimate for the super learner (and all algorithms in \code{SL.library}
#' for comparison), using a newly specified metalearning method. The weights for
#' each algorithm in \code{SL.library} are re-estimated using the new
#' metalearner, however the base learner fits are not regenerated, so this
#' function saves a lot of computation time as opposed to using the
#' \code{CV.SuperLearner} function with a new \code{method} argument.  The
#' output is identical to the output from the \code{CV.SuperLearner} function.
#'
#' @inheritParams CV.SuperLearner
#' @param object Fitted object from \code{CV.SuperLearner}.
#'
#' @inherit CV.SuperLearner return
#'
#' @author Erin LeDell \email{ledell@@berkeley.edu}
#'
#' @seealso \code{\link{recombineSL}}
#'
#' @keywords models
#'
#' @examples
#' \dontrun{
#' # Binary outcome example adapted from SuperLearner examples
#'
#' set.seed(1)
#' N <- 200
#' X <- matrix(rnorm(N*10), N, 10)
#' X <- as.data.frame(X)
#' Y <- rbinom(N, 1, plogis(.2*X[, 1] + .1*X[, 2] - .2*X[, 3] +
#'   .1*X[, 3]*X[, 4] - .2*abs(X[, 4])))
#'
#' SL.library <- c("SL.glmnet", "SL.glm", "SL.knn", "SL.gam", "SL.mean")
#'
#' # least squares loss function
#' set.seed(1) # for reproducibility
#' cvfit_nnls <- CV.SuperLearner(Y = Y, X = X, V = 10, SL.library = SL.library,
#'   verbose = TRUE, method = "method.NNLS", family = binomial())
#' cvfit_nnls$coef
#' #    SL.glmnet_All SL.glm_All  SL.knn_All SL.gam_All SL.mean_All
#' # 1      0.0000000 0.00000000 0.000000000  0.4143862   0.5856138
#' # 2      0.0000000 0.00000000 0.304802397  0.3047478   0.3904498
#' # 3      0.0000000 0.00000000 0.002897533  0.5544075   0.4426950
#' # 4      0.0000000 0.20322642 0.000000000  0.1121891   0.6845845
#' # 5      0.1743973 0.00000000 0.032471026  0.3580624   0.4350693
#' # 6      0.0000000 0.00000000 0.099881535  0.3662309   0.5338876
#' # 7      0.0000000 0.00000000 0.234876082  0.2942472   0.4708767
#' # 8      0.0000000 0.06424676 0.113988158  0.5600208   0.2617443
#' # 9      0.0000000 0.00000000 0.338030342  0.2762604   0.3857093
#' # 10     0.3022442 0.00000000 0.294226204  0.1394534   0.2640762
#'
#'
#' # negative log binomial likelihood loss function
#' cvfit_nnloglik <- recombineCVSL(cvfit_nnls, method = "method.NNloglik")
#' cvfit_nnloglik$coef
#' #    SL.glmnet_All SL.glm_All SL.knn_All SL.gam_All SL.mean_All
#' # 1      0.0000000  0.0000000 0.00000000  0.5974799  0.40252010
#' # 2      0.0000000  0.0000000 0.31177345  0.6882266  0.00000000
#' # 3      0.0000000  0.0000000 0.01377469  0.8544238  0.13180152
#' # 4      0.0000000  0.1644188 0.00000000  0.2387919  0.59678930
#' # 5      0.2142254  0.0000000 0.00000000  0.3729426  0.41283197
#' # 6      0.0000000  0.0000000 0.00000000  0.5847150  0.41528502
#' # 7      0.0000000  0.0000000 0.47538172  0.5080311  0.01658722
#' # 8      0.0000000  0.0000000 0.00000000  1.0000000  0.00000000
#' # 9      0.0000000  0.0000000 0.45384961  0.2923480  0.25380243
#' # 10     0.3977816  0.0000000 0.27927906  0.1606384  0.16230097
#'
#' # If we use the same seed as the original `cvfit_nnls`, then
#' # the recombineCVSL and CV.SuperLearner results will be identical
#' # however, the recombineCVSL version will be much faster since
#' # it doesn't have to re-fit all the base learners, V times each.
#' set.seed(1)
#' cvfit_nnloglik2 <- CV.SuperLearner(Y = Y, X = X, V = 10, SL.library = SL.library,
#'   verbose = TRUE, method = "method.NNloglik", family = binomial())
#' cvfit_nnloglik2$coef
#' #    SL.glmnet_All SL.glm_All SL.knn_All SL.gam_All SL.mean_All
#' # 1      0.0000000  0.0000000 0.00000000  0.5974799  0.40252010
#' # 2      0.0000000  0.0000000 0.31177345  0.6882266  0.00000000
#' # 3      0.0000000  0.0000000 0.01377469  0.8544238  0.13180152
#' # 4      0.0000000  0.1644188 0.00000000  0.2387919  0.59678930
#' # 5      0.2142254  0.0000000 0.00000000  0.3729426  0.41283197
#' # 6      0.0000000  0.0000000 0.00000000  0.5847150  0.41528502
#' # 7      0.0000000  0.0000000 0.47538172  0.5080311  0.01658722
#' # 8      0.0000000  0.0000000 0.00000000  1.0000000  0.00000000
#' # 9      0.0000000  0.0000000 0.45384961  0.2923480  0.25380243
#' # 10     0.3977816  0.0000000 0.27927906  0.1606384  0.16230097
#'
#' }

#' @export
recombineCVSL <- function(object, method = "method.NNloglik", verbose = FALSE, saveAll = TRUE, parallel = "seq") {
  # This function takes an existing CV.SuperLearner object and for each of the V
  # cross-validation folds, it re-fits the ensemble using a new metalearning method,
  # specified by the `method` argument, and returns a new CV.SuperLearner object.
  # This saves a lot of computation time since, for all V iterations, we can skip re-computing
  # the Z matrix of cv predicted values by cross-validating each base learner a second time.
  # The recombineCVSL function is a re-worked version of the original CV.SuperLearner function by Eric C. Polley.

  if (!inherits(object, "CV.SuperLearner")) {
    stop("The supplied 'object' is not of class CV.SuperLearner.")
  }

  if (is.character(method)) {
    if (exists(method, mode = 'list')) {
      method <- get(method, mode = 'list')
    }
    else if (exists(method, mode = 'function')) {
      method <- get(method, mode = 'function')()
    }
  }
  else if (is.function(method)) {
    method <- method()
  }

  if (!is.list(method)) {
    stop("method is not in the appropriate format. Check out help('method.template')")
  }

  if (!is.null(method$require)) {
    sapply(method$require, function(x) require(force(x), character.only = TRUE))
  }

  if (identical(object$method, method, ignore.environment=TRUE)) {
    # May want to modify this "if" statement because method.AUC with different args look the same here
    warning("The new method supplied is identical to the existing method.")
  }

  # get relevant objects from the CV.SuperLearner object
  call <- object$call
  library <- object$SL.library
  libraryNames <- object$libraryNames
  folds <- object$folds
  V <- object$V
  oldAllSL <- object$AllSL
  Y <- object$Y
  N <- length(object$Y)

  if (N != length(Y)) {
    stop("length(Y) does not match nrow(Z). Verify that Y is the same outcome variable that 'fit' was trained on.")
  }

  k <- nrow(library$library)
  AllSL <- vector('list', V)  #Need to relearn SL fits using recombineSL and update this list
  names(AllSL) <- paste("training", 1:V, sep=" ")
  SL.predict <- rep(NA, N)
  discreteSL.predict <- object$discreteSL.predict  #Maybe we should use the versions created below, and keep the NA versions here
  whichDiscreteSL <- object$whichDiscreteSL
  library.predict <- object$library.predict

  # Get required arguments
  family <- object$AllSL[[1]]$family
  vlist <- as.list(seq(V))
  names(vlist) <- names(folds)
  # Re-fit SuperLearner:
  .crossValFun <- function(v, folds, oldAllSL, Y, method, verbose, saveAll) {
    # Modified from equivalent internal SuperLearner function
    fit <- oldAllSL[[v]]
    valid <- folds[[v]]
    cvOutcome <- Y[-valid]
    fit.SL <- recombineSL(object = fit, Y = cvOutcome, method = method, verbose = verbose)

    list(cvAllSL = if (saveAll) fit.SL,
         cvSL.predict = fit.SL$SL.predict,
         cvdiscreteSL.predict = fit.SL$library.predict[, which.min(fit.SL$cvRisk)],
         cvwhichDiscreteSL = names(which.min(fit.SL$cvRisk)),
         cvlibrary.predict = fit.SL$library.predict,
         cvcoef = fit.SL$coef)
  }

  if (parallel == "seq") {
    cvList <- lapply(vlist, FUN = .crossValFun, folds = folds, oldAllSL = oldAllSL, Y = Y, method = method, verbose = verbose, saveAll = saveAll)
  }
  else if (parallel == 'multicore') {
    # not tested
    .SL.require('parallel')
    cvList <- parallel::mclapply(vlist, FUN = .crossValFun, folds = folds, oldAllSL = oldAllSL, Y = Y, method = method, verbose = verbose, saveAll = saveAll)
  }
  else if (inherits(parallel, 'cluster')) {
    # not tested
    cvList <- parallel::parLapply(parallel, x = vlist, fun = .crossValFun, folds = folds, oldAllSL = oldAllSL, Y = Y, method = method, verbose = verbose, saveAll = saveAll)
  }
  else {
    stop('parallel option was not recognized, use parallel = "seq" for sequential computation.')
  }

  AllSL <- lapply(cvList, '[[', 'cvAllSL')
  SL.predict[unlist(folds, use.names = FALSE)] <- unlist(lapply(cvList, '[[', 'cvSL.predict'), use.names = FALSE)
  discreteSL.predict[unlist(folds, use.names = FALSE)] <- unlist(lapply(cvList, '[[', 'cvdiscreteSL.predict'), use.names = FALSE)
  whichDiscreteSL <- lapply(cvList, '[[', 'cvwhichDiscreteSL')
  library.predict[unlist(folds, use.names = FALSE), ] <- do.call('rbind', lapply(cvList, '[[', 'cvlibrary.predict'))
  coef <- do.call('rbind', lapply(cvList, '[[', 'cvcoef'))
  colnames(coef) <- libraryNames

  object$AllSL <- AllSL
  object$SL.predict <- SL.predict
  object$discreteSL.predict <- discreteSL.predict
  object$whichDiscreteSL <- whichDiscreteSL
  object$library.predict <- library.predict
  object$coef <- coef
  object$method <- method

  object
}