#' Super Learner Prediction Function
#'
#' @description
#' \code{SuperLearner()} takes a training set pair (X,Y) and returns the predicted values
#' based on a validation set.
#'
#' @details
#' \code{SuperLearner()} fits the super learner prediction algorithm.  The
#' weights for each algorithm in \code{SL.library} is estimated, along with the
#' fit of each algorithm.
#'
#' \emph{The prescreen algorithms} These algorithms first rank the variables in
#' \code{X} based on either a univariate regression p-value of the
#' \code{randomForest} variable importance.  A subset of the variables in
#' \code{X} is selected based on a pre-defined cut-off.  With this subset of
#' the X variables, the algorithms in \code{SL.library} are then fit.
#'
#' The SuperLearner package contains a few prediction and screening algorithm
#' wrappers. The full list of wrappers can be viewed with
#' \code{listWrappers()}. The design of the \pkg{SuperLearner} package is such that
#' the user can easily add their own wrappers. We also maintain a website with
#' additional examples of wrapper functions at
#' \url{https://github.com/ecpolley/SuperLearnerExtra}.
#'
#' @param Y The outcome in the training data set. Must be a numeric vector.
#' @param X The predictor variables in the training data set, usually a
#' data.frame.
#' @param newX The predictor variables in the validation data set. The
#' structure should match X. If missing, uses X for newX.
#' @param SL.library Either a character vector of prediction algorithms or a
#' list containing character vectors. See details below for examples on the
#' structure. A list of functions included in the SuperLearner package can be
#' found with \code{listWrappers()}.
#' @param verbose logical; TRUE for printing progress during the computation
#' (helpful for debugging).
#' @param family Currently allows \code{gaussian} or \code{binomial} to
#' describe the error distribution. Link function information will be ignored
#' and should be contained in the method argument below.
#' @param method A list (or a function to create a list) containing details on
#' estimating the coefficients for the super learner and the model to combine
#' the individual algorithms in the library. See \code{?method.template} for
#' details.  Currently, the built in options are either "method.NNLS" (the
#' default), "method.NNLS2", "method.NNloglik", "method.CC_LS",
#' "method.CC_nloglik", or "method.AUC".  NNLS and NNLS2 are non-negative least
#' squares based on the Lawson-Hanson algorithm and the dual method of Goldfarb
#' and Idnani, respectively.  NNLS and NNLS2 will work for both gaussian and
#' binomial outcomes.  NNloglik is a non-negative binomial likelihood
#' maximization using the BFGS quasi-Newton optimization method. NN* methods
#' are normalized so weights sum to one. CC_LS uses Goldfarb and Idnani's
#' quadratic programming algorithm to calculate the best convex combination of
#' weights to minimize the squared error loss. CC_nloglik calculates the convex
#' combination of weights that minimize the negative binomial log likelihood on
#' the logistic scale using the sequential quadratic programming algorithm.
#' AUC, which only works for binary outcomes, uses the Nelder-Mead method via
#' the optim function to minimize rank loss (equivalent to maximizing AUC).
#' @param id Optional cluster identification variable. For the cross-validation
#' splits, \code{id} forces observations in the same cluster to be in the same
#' validation fold. \code{id} is passed to the prediction and screening
#' algorithms in SL.library, but be sure to check the individual wrappers as
#' many of them ignore the information.
#' @param obsWeights Optional observation weights variable. As with \code{id}
#' above, \code{obsWeights} is passed to the prediction and screening
#' algorithms, but many of the built in wrappers ignore (or can't use) the
#' information. If you are using observation weights, make sure the library you
#' specify uses the information.
#' @param control A list of parameters to control the estimation process.
#' Parameters include \code{saveFitLibrary} and \code{trimLogit}. See
#' \code{\link{SuperLearner.control}} for details.
#' @param cvControl A list of parameters to control the cross-validation
#' process. Parameters include \code{V}, \code{stratifyCV}, \code{shuffle} and
#' \code{validRows}. See \code{\link{SuperLearner.CV.control}} for details.
#' @param env Environment containing the learner functions. Defaults to the
#' calling environment.
#'
#' @returns
#' \item{call}{ The matched call. }
#' \item{libraryNames}{ A character
#' vector with the names of the algorithms in the library. The format is
#' 'predictionAlgorithm_screeningAlgorithm' with '_All' used to denote the
#' prediction algorithm run on all variables in X. }
#' \item{SL.library}{ Returns \code{SL.library} in the same format as the argument with the same name
#' above. }
#' \item{SL.predict}{ The predicted values from the super learner for
#' the rows in \code{newX}. }
#' \item{coef}{ Coefficients for the super learner.}
#' \item{library.predict}{ A matrix with the predicted values from each
#' algorithm in \code{SL.library} for the rows in \code{newX}. }
#' \item{Z}{ The
#' Z matrix (the cross-validated predicted values for each algorithm in
#' \code{SL.library}). }
#' \item{cvRisk}{ A numeric vector with the V-fold
#' cross-validated risk estimate for each algorithm in \code{SL.library}. Note
#' that this does not contain the CV risk estimate for the SuperLearner, only
#' the individual algorithms in the library. }
#' \item{family}{ Returns the
#' \code{family} value from above }
#' \item{fitLibrary}{ A list with the fitted
#' objects for each algorithm in \code{SL.library} on the full training data
#' set. }
#' \item{cvFitLibrary}{ A list with fitted objects for each algorithm in
#' \code{SL.library} on each of \code{V} different training data sets.  }
#' \item{varNames}{ A character vector with the names of the variables in
#' \code{X}. }
#' \item{validRows}{ A list containing the row numbers for the
#' V-fold cross-validation step. }
#' \item{method}{ A list with the method
#' functions. }
#' \item{whichScreen}{ A logical matrix indicating which variables
#' passed each screening algorithm. }
#' \item{control}{ The \code{control} list.
#' }
#' \item{cvControl}{ The \code{cvControl} list. }
#' \item{errorsInCVLibrary}{ A
#' logical vector indicating if any algorithms experienced an error within the
#' CV step. }
#' \item{errorsInLibrary}{ A logical vector indicating if any
#' algorithms experienced an error on the full data. }
#' \item{env}{ Environment
#' passed into function which will be searched to find the learner functions.
#' Defaults to the calling environment.  }
#' \item{times}{ A list that contains
#' the execution time of the SuperLearner, plus separate times for model
#' fitting and prediction.  }
#'
#' @author Eric C Polley \email{epolley@@uchicago.edu}
#'
#' @references
#' van der Laan, M. J., Polley, E. C. and Hubbard, A. E. (2008) Super Learner, \emph{Statistical Applications of Genetics and Molecular Biology}, \bold{6}, article 25.
#'
#' @seealso [CV.SuperLearner()], [SampleSplitSuperLearner()]
#'
#' @keywords models
#'
#' @examples
#'
#' \dontrun{
#' ## simulate data
#' set.seed(23432)
#' ## training set
#' n <- 500
#' p <- 50
#' X <- matrix(rnorm(n*p), nrow = n, ncol = p)
#' colnames(X) <- paste("X", 1:p, sep="")
#' X <- data.frame(X)
#' Y <- X[, 1] + sqrt(abs(X[, 2] * X[, 3])) + X[, 2] - X[, 3] + rnorm(n)
#'
#' ## test set
#' m <- 1000
#' newX <- matrix(rnorm(m*p), nrow = m, ncol = p)
#' colnames(newX) <- paste("X", 1:p, sep="")
#' newX <- data.frame(newX)
#' newY <- newX[, 1] + sqrt(abs(newX[, 2] * newX[, 3])) + newX[, 2] -
#'   newX[, 3] + rnorm(m)
#'
#' # generate Library and run Super Learner
#' SL.library <- c("SL.glm", "SL.randomForest", "SL.gam",
#'   "SL.polymars", "SL.mean")
#' test <- SuperLearner(Y = Y, X = X, newX = newX, SL.library = SL.library,
#'   verbose = TRUE, method = "method.NNLS")
#' test
#'
#' # library with screening
#' SL.library <- list(c("SL.glmnet", "All"), c("SL.glm", "screen.randomForest",
#'   "All", "screen.SIS"), "SL.randomForest", c("SL.polymars", "All"), "SL.mean")
#' test <- SuperLearner(Y = Y, X = X, newX = newX, SL.library = SL.library,
#'   verbose = TRUE, method = "method.NNLS")
#' test
#'
#' # binary outcome
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
#' test.NNLS <- SuperLearner(Y = Y, X = X, SL.library = SL.library,
#'   verbose = TRUE, method = "method.NNLS", family = binomial())
#' test.NNLS
#'
#' # negative log binomial likelihood loss function
#' test.NNloglik <- SuperLearner(Y = Y, X = X, SL.library = SL.library,
#'   verbose = TRUE, method = "method.NNloglik", family = binomial())
#' test.NNloglik
#'
#' # 1 - AUC loss function
#' test.AUC <- SuperLearner(Y = Y, X = X, SL.library = SL.library,
#'   verbose = TRUE, method = "method.AUC", family = binomial())
#' test.AUC
#'
#' # 2
#' # adapted from library(SIS)
#' set.seed(1)
#' # training
#' b <- c(2, 2, 2, -3*sqrt(2))
#' n <- 150
#' p <- 200
#' truerho <- 0.5
#' corrmat <- diag(rep(1-truerho, p)) + matrix(truerho, p, p)
#' corrmat[, 4] = sqrt(truerho)
#' corrmat[4, ] = sqrt(truerho)
#' corrmat[4, 4] = 1
#' cholmat <- chol(corrmat)
#' x <- matrix(rnorm(n*p, mean=0, sd=1), n, p)
#' x <- x %*% cholmat
#' feta <- x[, 1:4] %*% b
#' fprob <- exp(feta) / (1 + exp(feta))
#' y <- rbinom(n, 1, fprob)
#'
#' # test
#' m <- 10000
#' newx <- matrix(rnorm(m*p, mean=0, sd=1), m, p)
#' newx <- newx %*% cholmat
#' newfeta <- newx[, 1:4] %*% b
#' newfprob <- exp(newfeta) / (1 + exp(newfeta))
#' newy <- rbinom(m, 1, newfprob)
#'
#' DATA2 <- data.frame(Y = y, X = x)
#' newDATA2 <- data.frame(Y = newy, X=newx)
#'
#' create.SL.knn <- function(k = c(20, 30)) {
#'   for(mm in seq(length(k))){
#'     eval(parse(text = paste('SL.knn.', k[mm], '<- function(..., k = ', k[mm],
#'       ') SL.knn(..., k = k)', sep = '')), envir = .GlobalEnv)
#'   }
#'   invisible(TRUE)
#' }
#' create.SL.knn(c(20, 30, 40, 50, 60, 70))
#'
#' # library with screening
#' SL.library <- list(c("SL.glmnet", "All"), c("SL.glm", "screen.randomForest"),
#'   "SL.randomForest", "SL.knn", "SL.knn.20", "SL.knn.30", "SL.knn.40",
#'   "SL.knn.50", "SL.knn.60", "SL.knn.70",
#'   c("SL.polymars", "screen.randomForest"))
#' test <- SuperLearner(Y = DATA2$Y, X = DATA2[, -1], newX = newDATA2[, -1],
#'   SL.library = SL.library, verbose = TRUE, family = binomial())
#' test
#'
#' ## examples with multicore
#' set.seed(23432, "L'Ecuyer-CMRG")  # use L'Ecuyer for multicore seeds. see ?set.seed for details
#' ## training set
#' n <- 500
#' p <- 50
#' X <- matrix(rnorm(n*p), nrow = n, ncol = p)
#' colnames(X) <- paste("X", 1:p, sep="")
#' X <- data.frame(X)
#' Y <- X[, 1] + sqrt(abs(X[, 2] * X[, 3])) + X[, 2] - X[, 3] + rnorm(n)
#'
#' ## test set
#' m <- 1000
#' newX <- matrix(rnorm(m*p), nrow = m, ncol = p)
#' colnames(newX) <- paste("X", 1:p, sep="")
#' newX <- data.frame(newX)
#' newY <- newX[, 1] + sqrt(abs(newX[, 2] * newX[, 3])) + newX[, 2] - newX[, 3] + rnorm(m)
#'
#' # generate Library and run Super Learner
#' SL.library <- c("SL.glm", "SL.randomForest", "SL.gam",
#'   "SL.polymars", "SL.mean")
#'
#' testMC <- mcSuperLearner(Y = Y, X = X, newX = newX, SL.library = SL.library,
#'   method = "method.NNLS")
#' testMC
#'
#' ## examples with snow
#' library(parallel)
#' cl <- makeCluster(2, type = "PSOCK") # can use different types here
#' clusterSetRNGStream(cl, iseed = 2343)
#' # make SL functions available on the clusters, use assignment to avoid printing
#' foo <- clusterEvalQ(cl, library(SuperLearner))
#' testSNOW <- snowSuperLearner(cluster = cl, Y = Y, X = X, newX = newX,
#'   SL.library = SL.library, method = "method.NNLS")
#' testSNOW
#' stopCluster(cl)
#'
#' ## snow example with user-generated wrappers
#' # If you write your own wrappers and are using snowSuperLearner()
#' # These new wrappers need to be added to the SuperLearner namespace and exported to the clusters
#' # Using a simple example here, but can define any new SuperLearner wrapper
#' my.SL.wrapper <- function(...) SL.glm(...)
#' # assign function into SuperLearner namespace
#' environment(my.SL.wrapper) <-asNamespace("SuperLearner")
#'
#' cl <- makeCluster(2, type = "PSOCK") # can use different types here
#' clusterSetRNGStream(cl, iseed = 2343)
#' # make SL functions available on the clusters, use assignment to avoid printing
#' foo <- clusterEvalQ(cl, library(SuperLearner))
#' clusterExport(cl, c("my.SL.wrapper"))  # copy the function to all clusters
#' testSNOW <- snowSuperLearner(cluster = cl, Y = Y, X = X, newX = newX,
#'   SL.library = c("SL.glm", "SL.mean", "my.SL.wrapper"), method = "method.NNLS")
#' testSNOW
#' stopCluster(cl)
#'
#' ## timing
#' replicate(5, system.time(SuperLearner(Y = Y, X = X, newX = newX,
#'   SL.library = SL.library, method = "method.NNLS")))
#'
#' replicate(5, system.time(mcSuperLearner(Y = Y, X = X, newX = newX,
#'   SL.library = SL.library, method = "method.NNLS")))
#'
#' cl <- makeCluster(2, type = 'PSOCK')
#' # make SL functions available on the clusters, use assignment to avoid printing
#' foo <- clusterEvalQ(cl, library(SuperLearner))
#' replicate(5, system.time(snowSuperLearner(cl, Y = Y, X = X, newX = newX,
#'   SL.library = SL.library, method = "method.NNLS")))
#' stopCluster(cl)
#'
#' }

#' @export
SuperLearner <- function(Y, X, newX = NULL, family = gaussian(), SL.library,
                         method = 'method.NNLS', id = NULL, verbose = FALSE, control = list(),
                         cvControl = list(), obsWeights = NULL, env = parent.frame()) {

    # Begin timing how long SuperLearner takes to execute.
    time_start = proc.time()

    if (is.character(method)) {
        if (exists(method, mode = 'list')) {
            method <- get(method, mode = 'list')
        } else if (exists(method, mode = 'function')) {
            method <- get(method, mode = 'function')()
        }
    } else if (is.function(method)) {
        method <- method()
    }
    if(!is.list(method)) {
        stop("method is not in the appropriate format. Check out help('method.template')")
    }
    if(!is.null(method$require)) {
        sapply(method$require, function(x) require(force(x), character.only = TRUE))
    }
    # get defaults for controls and make sure in correct format
    control <- do.call('SuperLearner.control', control)
    cvControl <- do.call('SuperLearner.CV.control', cvControl)

    # put together the library
    # should this be in a new environment?
    library <- .createLibrary(SL.library)
    .check.SL.library(library = c(unique(library$library$predAlgorithm), library$screenAlgorithm))

    call <- match.call(expand.dots = TRUE)
    # should we be checking X and newX for data.frame?
    # data.frame not required, but most of the built-in wrappers assume a data.frame
    if(!inherits(X, 'data.frame')) message('X is not a data frame. Check the algorithms in SL.library to make sure they are compatible with non data.frame inputs')
    varNames <- colnames(X)
    N <- dim(X)[1L]
    p <- dim(X)[2L]
    k <- nrow(library$library)
    kScreen <- length(library$screenAlgorithm)
    Z <- matrix(NA, N, k)
    libraryNames <- paste(library$library$predAlgorithm, library$screenAlgorithm[library$library$rowScreen], sep="_")

    if(p < 2 & !identical(library$screenAlgorithm, "All")) {
        warning('Screening algorithms specified in combination with single-column X.')
    }

    # put fitLibrary in it's own environment to locate later
    fitLibEnv <- new.env()
    assign('fitLibrary', vector('list', length = k), envir = fitLibEnv)
    assign('libraryNames', libraryNames, envir = fitLibEnv)
    evalq(names(fitLibrary) <- libraryNames, envir = fitLibEnv)

    # errors* records if an algorithm stops either in the CV step and/or in full data
    errorsInCVLibrary <- rep(0, k)
    errorsInLibrary <- rep(0, k)

    # if newX is missing, use X
    if(is.null(newX)) {
        newX <- X
    }
    # Are these checks still required?
    if(!identical(colnames(X), colnames(newX))) {
        stop("The variable names and order in newX must be identical to the variable names and order in X")
    }
    if (sum(is.na(X)) > 0 | sum(is.na(newX)) > 0 | sum(is.na(Y)) > 0) {
        stop("missing data is currently not supported. Check Y, X, and newX for missing values")
    }
    if (!is.numeric(Y)) {
        stop("the outcome Y must be a numeric vector")
    }
    # family can be either character or function, so these lines put everything together (code from glm())
    if(is.character(family))
        family <- get(family, mode="function", envir=parent.frame())
    if(is.function(family))
        family <- family()
    if (is.null(family$family)) {
        print(family)
        stop("'family' not recognized")
    }

    if (family$family != "binomial" & isTRUE("cvAUC" %in% method$require)){
        stop("'method.AUC' is designed for the 'binomial' family only")
    }

    # create CV folds
    validRows <- CVFolds(N = N, id = id, Y = Y, cvControl = cvControl)

    # test id
    if(is.null(id)) {
        id <- seq(N)
    }
    if(!identical(length(id), N)) {
        stop("id vector must have the same dimension as Y")
    }
    # test observation weights
    if(is.null(obsWeights)) {
        obsWeights <- rep(1, N)
    }
    if(!identical(length(obsWeights), N)) {
        stop("obsWeights vector must have the same dimension as Y")
    }

    # create function for the cross-validation step:
    .crossValFUN <- function(valid, Y, dataX, id, obsWeights, library,
                             kScreen, k, p, libraryNames, saveCVFitLibrary) {
        tempLearn <- dataX[-valid, , drop = FALSE]
        tempOutcome <- Y[-valid]
        tempValid <- dataX[valid, , drop = FALSE]
        tempWhichScreen <- matrix(NA, nrow = kScreen, ncol = p)
        tempId <- id[-valid]
        tempObsWeights <- obsWeights[-valid]

        # should this be converted to a lapply also?
        for(s in seq(kScreen)) {
            screen_fn = get(library$screenAlgorithm[s], envir = env)
            testScreen <- try(do.call(screen_fn, list(Y = tempOutcome, X = tempLearn, family = family, id = tempId, obsWeights = tempObsWeights)))
            if(inherits(testScreen, "try-error")) {
                warning(paste("replacing failed screening algorithm,", library$screenAlgorithm[s], ", with All()", "\n "))
                tempWhichScreen[s, ] <- TRUE
            } else {
                tempWhichScreen[s, ] <- testScreen
            }
            if(verbose) {
                message(paste("Number of covariates in ", library$screenAlgorithm[s], " is: ", sum(tempWhichScreen[s, ]), sep = ""))
            }
        } #end screen

        # should this be converted to a lapply also?
        out <- matrix(NA, nrow = nrow(tempValid), ncol = k)
        if(saveCVFitLibrary){
            model_out <- vector(mode = "list", length = k)
        }else{
            model_out <- NULL
        }

        for(s in seq(k)) {
            pred_fn = get(library$library$predAlgorithm[s], envir = env)
            testAlg <- try(do.call(pred_fn, list(Y = tempOutcome, X = subset(tempLearn, select = tempWhichScreen[library$library$rowScreen[s], ], drop=FALSE), newX = subset(tempValid, select = tempWhichScreen[library$library$rowScreen[s], ], drop=FALSE), family = family, id = tempId, obsWeights = tempObsWeights)))
            if(inherits(testAlg, "try-error")) {
                warning(paste("Error in algorithm", library$library$predAlgorithm[s], "\n  The Algorithm will be removed from the Super Learner (i.e. given weight 0) \n" ))
            # errorsInCVLibrary[s] <<- 1
            } else {
                out[, s] <- testAlg$pred
                if(saveCVFitLibrary){
                    model_out[[s]] <- testAlg$fit
                }
            }
            if (verbose) message(paste("CV", libraryNames[s]))
        } #end library
        if(saveCVFitLibrary){
            names(model_out) <- libraryNames
        }
        invisible(list(out = out, model_out = model_out))
    }
    # the lapply performs the cross-validation steps to create Z
    # additional steps to put things in the correct order
    # rbind unlists the output from lapply
    # need to unlist folds to put the rows back in the correct order
    time_train_start = proc.time()

    crossValFUN_out <- lapply(validRows, FUN = .crossValFUN,
                              Y = Y, dataX = X, id = id,
                              obsWeights = obsWeights,
                              library = library, kScreen = kScreen,
                              k = k, p = p, libraryNames = libraryNames,
                              saveCVFitLibrary = control$saveCVFitLibrary)
    Z[unlist(validRows, use.names = FALSE), ] <- do.call('rbind', lapply(crossValFUN_out, "[[", "out"))
    if(control$saveCVFitLibrary){
        cvFitLibrary <- lapply(crossValFUN_out, "[[", "model_out")
    }else{
        cvFitLibrary <- NULL
    }
    # Check for errors. If any algorithms had errors, replace entire column with
    # 0 even if error is only in one fold.
    errorsInCVLibrary <- apply(Z, 2, function(x) anyNA(x))
    if (sum(errorsInCVLibrary) > 0) {
        Z[, as.logical(errorsInCVLibrary)] <- 0
    }
    if (all(Z == 0)) {
        stop("All algorithms dropped from library")
    }

    # Compute weights for each algorithm in library.
    getCoef <- method$computeCoef(Z = Z, Y = Y, libraryNames = libraryNames,
                                  obsWeights = obsWeights, control = control,
                                  verbose = verbose,
                                  errorsInLibrary = errorsInCVLibrary)
    coef <- getCoef$coef
    names(coef) <- libraryNames

    time_train = proc.time() - time_train_start

    # Set a default in case the method does not return the optimizer result.
    if (!("optimizer" %in% names(getCoef))) {
        getCoef["optimizer"] <- NA
    }

    # now fit all algorithms in library on entire learning data set and predict on newX
    m <- dim(newX)[1L]
    predY <- matrix(NA, nrow = m, ncol = k)
    # whichScreen <- matrix(NA, nrow = kScreen, ncol = p)

    .screenFun <- function(fun, list) {
        screen_fn = get(fun, envir = env)
        testScreen <- try(do.call(screen_fn, list))
        if (inherits(testScreen, "try-error")) {
            warning(paste("replacing failed screening algorithm,", fun, ", with All() in full data", "\n "))
            out <- rep(TRUE, ncol(list$X))
        } else {
            out <- testScreen
        }
        return(out)
    }

    time_predict_start = proc.time()

    whichScreen <- sapply(library$screenAlgorithm, FUN = .screenFun, list = list(Y = Y, X = X, family = family, id = id, obsWeights = obsWeights), simplify = FALSE)
    whichScreen <- do.call(rbind, whichScreen)

    # change to sapply?
    # for(s in 1:k) {
    #   testAlg <- try(do.call(library$library$predAlgorithm[s], list(Y = Y, X = subset(X, select = whichScreen[library$library$rowScreen[s], ], drop=FALSE), newX = subset(newX, select = whichScreen[library$library$rowScreen[s], ], drop=FALSE), family = family, id = id, obsWeights = obsWeights)))
    #   if(inherits(testAlg, "try-error")) {
    #     warning(paste("Error in algorithm", library$library$predAlgorithm[s], " on full data", "\n  The Algorithm will be removed from the Super Learner (i.e. given weight 0) \n" ))
    #     errorsInLibrary[s] <- 1
    #   } else {
    #     predY[, s] <- testAlg$pred
    #   }
    #   if(control$saveFitLibrary) {
    #     fitLibrary[[s]] <- testAlg$fit
    #   }
    #   if(verbose) {
    #     message(paste("full", libraryNames[s]))
    #   }
    # }
    .predFun <- function(index, lib, Y, dataX, newX, whichScreen, family, id, obsWeights, verbose, control, libraryNames) {
        pred_fn = get(lib$predAlgorithm[index], envir = env)
        testAlg <- try(do.call(pred_fn, list(Y = Y,
                                             X = subset(dataX,
                                                        select = whichScreen[lib$rowScreen[index], ], drop=FALSE),
                                             newX = subset(newX, select = whichScreen[lib$rowScreen[index], ], drop=FALSE),
                                             family = family, id = id, obsWeights = obsWeights)))
        # testAlg <- try(do.call(lib$predAlgorithm[index], list(Y = Y, X = dataX[, whichScreen[lib$rowScreen[index], drop = FALSE]], newX = newX[, whichScreen[lib$rowScreen[index], drop = FALSE]], family = family, id = id, obsWeights = obsWeights)))
        if (inherits(testAlg, "try-error")) {
            warning(paste("Error in algorithm", lib$predAlgorithm[index], " on full data", "\n  The Algorithm will be removed from the Super Learner (i.e. given weight 0) \n" ))
            out <- rep.int(NA, times = nrow(newX))
        } else {
            out <- testAlg$pred
            if (control$saveFitLibrary) {
                eval(bquote(fitLibrary[[.(index)]] <- .(testAlg$fit)), envir = fitLibEnv)
            }
        }
        if (verbose) {
            message(paste("full", libraryNames[index]))
        }
        invisible(out)
    }


    predY <- do.call('cbind', lapply(seq(k), FUN = .predFun,
                                     lib = library$library, Y = Y, dataX = X,
                                     newX = newX, whichScreen = whichScreen,
                                     family = family, id = id,
                                     obsWeights = obsWeights, verbose = verbose,
                                     control = control,
                                     libraryNames = libraryNames))

    # check for errors
    errorsInLibrary <- apply(predY, 2, function(algorithm) anyNA(algorithm))
    if (sum(errorsInLibrary) > 0) {
        if (sum(coef[as.logical(errorsInLibrary)]) > 0) {
            warning(paste0("Re-running estimation of coefficients removing failed algorithm(s)\n",
                           "Original coefficients are: \n", paste(coef, collapse = ", "), "\n"))
            Z[, as.logical(errorsInLibrary)] <- 0
            if (all(Z == 0)) {
                stop("All algorithms dropped from library")
            }
            getCoef <- method$computeCoef(Z = Z, Y = Y, libraryNames = libraryNames,
                                          obsWeights = obsWeights, control = control,
                                          verbose = verbose,
                                          errorsInLibrary = errorsInLibrary)
            coef <- getCoef$coef
            names(coef) <- libraryNames
        } else {
            warning("Coefficients already 0 for all failed algorithm(s)")
        }
    }

    # Compute super learner predictions on newX.
    getPred <- method$computePred(predY = predY, coef = coef, control = control)

    time_predict = proc.time() - time_predict_start

    # Add names of algorithms to the predictions.
    colnames(predY) <- libraryNames

    # Clean up when errors in library.
    if(sum(errorsInCVLibrary) > 0) {
        getCoef$cvRisk[as.logical(errorsInCVLibrary)] <- NA
    }

    # Finish timing the full SuperLearner execution.
    time_end = proc.time()

    # Compile execution times.
    times = list(everything = time_end - time_start,
                 train = time_train,
                 predict = time_predict)

    # Put everything together in a list.
    out <- list(
        call = call,
        libraryNames = libraryNames,
        SL.library = library,
        SL.predict = getPred,
        coef = coef,
        library.predict = predY,
        Z = Z,
        cvRisk = getCoef$cvRisk,
        family = family,
        fitLibrary = get('fitLibrary', envir = fitLibEnv),
        cvFitLibrary = cvFitLibrary,
        varNames = varNames,
        validRows = validRows,
        method = method,
        whichScreen = whichScreen,
        control = control,
        cvControl = cvControl,
        errorsInCVLibrary = errorsInCVLibrary,
        errorsInLibrary = errorsInLibrary,
        metaOptimizer = getCoef$optimizer,
        env = env,
        times = times
    )
    class(out) <- c("SuperLearner")
    return(out)
}
