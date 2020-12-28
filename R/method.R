# outline for SuperLearner methods
# these should always have class 'SL.method'
#
# The SuperLearner method is a coupling of the estimation algorithm for the algorithm weights (coefficients) and the model to combine the algorithms
#
# 2 parts need to be included:
#   1) compute coefficients
#   2) compute predictions

method.template <- function() {
  out <- list(
  # require allows you to pass a character vector with required packages
  # use NULL if no required packages
  require = NULL,
  # computeCoef is a function that returns a list with three elements:
  # 1) coef: the weights (coefficients) for each algorithm
  # 2) cvRisk: the V-fold CV risk for each algorithm
  # 3) optimizer: (optional) the result object from the optimization of the weights.
  computeCoef = function(Z, Y, libraryNames, obsWeights, control, verbose, ...) {
    cvRisk <- numeric()
    coef <- numeric()
    out <- list(cvRisk = cvRisk, coef = coef, optimizer = NULL)
    return(out)
  },
  # computePred is a function that takes the weights and the predicted values from each algorithm in the library and combines them based on the model to output the super learner predicted values
  computePred = function(predY, coef, control, ...) {
    out <- crossprod(t(predY), coef)
    return(out)
  }
  )
  invisible(out)
}

write.method.template <- function(file = '', ...) {
  cat('method.template <- function() {\n  out <- list(\n    # require allows you to pass a character vector with required packages\n    # use NULL if no required packages\n    require = NULL,\n\n    # computeCoef is a function that returns a list with two elements:\n    # 1) coef: the weights (coefficients) for each algorithm\n    # 2) cvRisk: the V-fold CV risk for each algorithm\n    computeCoef = function(Z, Y, libraryNames, obsWeights, control, verbose, ...) {\n      cvRisk <- numeric()\n      coef <- numeric()\n      out <- list(cvRisk = cvRisk, coef = coef)\n      return(out)\n    },\n\n    # computePred is a function that takes the weights and the predicted values\n    # from each algorithm in the library and combines them based on the model to\n    # output the super learner predicted values\n    computePred = function(predY, coef, control, ...) {\n      out <- crossprod(t(predY), coef)\n      return(out)\n    }\n    )\n    invisible(out)\n  }', file = file, ...)
}

# examples:
method.NNLS <- function() {
  out <- list(
    require = 'nnls',
    computeCoef = function(Z, Y, libraryNames, verbose, obsWeights, ...) {
      # compute cvRisk
      cvRisk <- apply(Z, 2, function(x) mean(obsWeights * (x - Y)^2))
      names(cvRisk) <- libraryNames

      # compute coef
      fit.nnls <- nnls(sqrt(obsWeights) * Z, sqrt(obsWeights) * Y)
      if (verbose) {
        message(paste("Non-Negative least squares convergence:", fit.nnls$mode == 1))
      }

      initCoef <- coef(fit.nnls)
      initCoef[is.na(initCoef)] <- 0.0
      # normalize so sum(coef) = 1 if possible
      if (sum(initCoef) > 0) {
        coef <- initCoef / sum(initCoef)
      } else {
        warning("All algorithms have zero weight", call. = FALSE)
        coef <- initCoef
      }
      out <- list(cvRisk = cvRisk, coef = coef, optimizer = fit.nnls)
      return(out)
    },

    computePred = function(predY, coef, ...) {
      if (sum(coef != 0) == 0) {
        warning("All metalearner coefficients are zero, predictions will all be equal to 0", call. = FALSE)
        out <- rep(0, nrow(predY))
      } else {
        # Restrict crossproduct to learners with non-zero coefficients.
        out <- crossprod(t(predY[, coef != 0, drop = FALSE]), coef[coef != 0])
      }
      return(out)
    }
  )
  invisible(out)
}

method.NNLS2 <- function() {
  out <- list(
  require = 'quadprog',
  computeCoef = function(Z, Y, libraryNames, verbose, obsWeights,
                         errorsInLibrary = NULL, ...) {
    # compute cvRisk
    cvRisk <- apply(Z, 2, function(x) mean(obsWeights * (x - Y)^2))
    names(cvRisk) <- libraryNames
    # compute coef
    .NNLS <- function(x, y, wt, errorsInLibrary = NULL) {
      wX <- sqrt(wt) * x
      wY <- sqrt(wt) * y
      # Z'Z = n * cov(Z)
      D <- t(wX) %*% wX
      d <- t(t(wY) %*% wX)
      A <- diag(ncol(wX))
      b <- rep(0, ncol(wX))
      # This will give an error if cov(Z) is singular, meaning at least two
      # columns are linearly dependent.
      # TODO: This will also error if any learner failed. Fix this.
      fit <- quadprog::solve.QP(Dmat = D, dvec = d, Amat = t(A), bvec = b, meq=0)
      invisible(fit)
    }
    fit.nnls <- .NNLS(x = Z, y = Y, wt = obsWeights,
                      errorsInLibrary = errorsInLibrary)
    initCoef <- fit.nnls$solution
    initCoef[initCoef < 0] <- 0.0
    initCoef[is.na(initCoef)] <- 0.0
    # normalize so sum(coef) = 1 if possible
    if(sum(initCoef) > 0) {
      coef <- initCoef/sum(initCoef)
    } else {
      warning("All algorithms have zero weight", call. = FALSE)
      coef <- initCoef
    }
    out <- list(cvRisk = cvRisk, coef = coef, optimizer = fit.nnls)
    return(out)
  },
  computePred = function(predY, coef, ...) {
    out <- crossprod(t(predY), coef)
    return(out)
  }
  )
  invisible(out)
}

method.NNloglik <- function() {
  out <- list(
    require = NULL,
    computeCoef = function(Z, Y, libraryNames, verbose, obsWeights, control,
                           errorsInLibrary = NULL, ...) {
      # compute cvRisk
      cvRisk <- apply(Z, 2, function(x) { -mean(obsWeights * ifelse(Y, log(x), log(1-x))) } )
      names(cvRisk) <- libraryNames
      # compute coef
      .NNloglik <- function(x, y, wt, start = rep(0, ncol(x))) {
        # adapted from MASS pg 445
        fmin <- function(beta, X, y, w) {
          p <- plogis(crossprod(t(X), beta))
          -sum(2 * w * ifelse(y, log(p), log(1-p)))
        }
        gmin <- function(beta, X, y, w) {
          eta <- X %*% beta
          p <- plogis(eta)
          -2 * t(w * dlogis(eta) * ifelse(y, 1/p, -1/(1-p))) %*% X
        }
        fit <- optim(start, fmin, gmin, X = x, y = y, w = wt, method = "L-BFGS-B", lower = 0, ...)
        invisible(fit)
      }
      tempZ <- trimLogit(Z, trim = control$trimLogit)
      fit.nnloglik <- .NNloglik(x = tempZ, y = Y, wt = obsWeights)
      if (verbose) {
        message(paste("Non-Negative log-likelihood convergence: ", fit.nnloglik$convergence == 0))
      }
      initCoef <- fit.nnloglik$par
      initCoef[initCoef < 0] <- 0.0
      initCoef[is.na(initCoef)] <- 0.0

      # Any algorithms with NA cvRisk will be restricted to 0 coefficient.
      # Otherwise algorithms with NA risk and all NA predictions can still receive
      # a positive coefficient. This does not bode well for this optimization
      # algorithm but we will handle anyway.
      if (sum(errorsInLibrary) > 0) {
        initCoef[errorsInLibrary] = 0
      }
      # normalize so sum(coef) = 1 if possible
      if (sum(initCoef) > 0) {
        coef <- initCoef / sum(initCoef)
      } else {
        warning("All algorithms have zero weight", call. = FALSE)
        coef <- initCoef
      }
      out <- list(cvRisk = cvRisk, coef = coef, optimizer = fit.nnloglik)
      return(out)
    },
    computePred = function(predY, coef, control, ...) {
      if (sum(coef != 0) == 0) {
        warning("All metalearner coefficients are zero, predictions will all be equal to 0", call. = FALSE)
        out <- rep(0, nrow(predY))
      } else {
        # Restrict crossproduct to learners with non-zero coefficients.
        out <- plogis(crossprod(t(trimLogit(predY[, coef != 0], trim = control$trimLogit)),
                              coef[coef != 0]))
      }
      return(out)
    }
  )
  invisible(out)
}

method.CC_LS <- function() {
    # Contributed by Sam Lendle
    # Edited by David Benkeser
  computeCoef = function(Z, Y, libraryNames, verbose, obsWeights,
                         errorsInLibrary = NULL, ...) {
    # compute cvRisk
    cvRisk <- apply(Z, 2, function(x) mean(obsWeights * (x - Y)^2))
    names(cvRisk) <- libraryNames
    # compute coef
    compute <- function(x, y, wt=rep(1, length(y))) {
      wX <- sqrt(wt) * x
      wY <- sqrt(wt) * y
      D <- crossprod(wX)
      d <- crossprod(wX, wY)
      A <- cbind(rep(1, ncol(wX)), diag(ncol(wX)))
      bvec <- c(1, rep(0, ncol(wX)))
      fit <- quadprog::solve.QP(Dmat=D, dvec=d, Amat=A, bvec=bvec, meq=1)
      invisible(fit)
    }
    modZ <- Z
    # check for columns of all zeros. assume these correspond
    # to errors that SuperLearner sets equal to 0. not a robust
    # solution, since in theory an algorithm could predict 0 for
    # all observations (e.g., SL.mean when all Y in training = 0)
    naCols <- which(apply(Z, 2, function(z){ all(z == 0 ) }))
    anyNACols <- length(naCols) > 0
    if(anyNACols){
        # if present, throw warning identifying learners
        warning(paste0(paste0(libraryNames[naCols],collapse = ", "), " have NAs.",
                       "Removing from super learner."))
    }
    # check for duplicated columns
    # set a tolerance level to avoid numerical instability
    tol <- 8
    dupCols <- which(duplicated(round(Z, tol), MARGIN = 2))
    anyDupCols <- length(dupCols) > 0 
    if(anyDupCols){
        # if present, throw warning identifying learners
        warning(paste0(paste0(libraryNames[dupCols],collapse = ", "), 
                       " are duplicates of previous learners.",
                       " Removing from super learner."))
    }
    # remove from Z if present
    if(anyDupCols | anyNACols){
        rmCols <- unique(c(naCols,dupCols))
        modZ <- Z[,-rmCols]
    }
    # compute coefficients on remaining columns
    fit <- compute(x = modZ, y = Y, wt = obsWeights)
    coef <- fit$solution
    if (anyNA(coef)) {
      warning("Some algorithms have weights of NA, setting to 0.")
      coef[is.na(coef)] = 0
    }
    # add in coefficients with 0 weights for algorithms with NAs
    if(anyDupCols | anyNACols){
        ind <- c(seq_along(coef), rmCols - 0.5)
        coef <- c(coef, rep(0, length(rmCols)))
        coef <- coef[order(ind)]
    }

    # Set very small coefficients to 0 and renormalize.
    coef[coef < 1.0e-4] <- 0
    coef <- coef / sum(coef)

    if(!sum(coef) > 0) warning("All algorithms have zero weight", call. = FALSE)
    list(cvRisk = cvRisk, coef = coef, optimizer = fit)
  }

  computePred = function(predY, coef, ...) {
   predY %*% matrix(coef)
  }

  out <- list(require = "quadprog",
              computeCoef = computeCoef,
              computePred = computePred)
  invisible(out)
}

method.CC_nloglik <- function() {
    # Contributed by Sam Lendle
    # Edited by David Benkeser
  computePred = function(predY, coef, control, ...) {
    if (sum(coef != 0) == 0) {
      warning("All metalearner coefficients are zero, predictions will all be 0", call. = FALSE)
    }
    plogis(trimLogit(predY[, coef != 0], trim = control$trimLogit) %*%
             matrix(coef[coef != 0]))
  }
  computeCoef = function(Z, Y, libraryNames, obsWeights, control, verbose, ...) {
    # check for duplicated columns
    # set a tolerance 
    tol <- 8
    dupCols <- which(duplicated(round(Z, tol), MARGIN = 2))
    anyDupCols <- length(dupCols) > 0 
    modZ <- Z
    if(anyDupCols){
        # if present, throw warning identifying learners
        warning(paste0(paste0(libraryNames[dupCols],collapse = ", "), 
                       " are duplicates of previous learners.",
                       " Removing from super learner."))
        modZ <- modZ[,-dupCols]
    }
    modlogitZ <- trimLogit(modZ, control$trimLogit)
    logitZ <- trimLogit(Z, control$trimLogit)

    cvRisk <- apply(logitZ, 2, function(x) -sum(2 * obsWeights *
                                       ifelse(Y, plogis(x, log.p=TRUE),
                                                 plogis(x, log.p=TRUE, lower.tail=FALSE))))
    names(cvRisk) <- libraryNames
    obj_and_grad <- function(y,x, w=NULL) {
        y <- y
        x <- x
      function(beta) {
        xB <- x %*% cbind(beta)
        loglik <- y * plogis(xB, log.p=TRUE) + (1-y) * plogis(xB, log.p=TRUE, lower.tail=FALSE)
        if (!is.null(w)) loglik <- loglik * w
        obj <- -2 * sum(loglik)
        p <- plogis(xB)
        grad <- if (is.null(w)) 2 * crossprod(x, cbind(p - y))
        else 2 * crossprod(x, w*cbind(p - y))
        list(objective=obj, gradient=grad)
      }
    }

    lower_bounds = rep(0, ncol(modZ))
    upper_bounds = rep(1, ncol(modZ))

    # Any algorithms with NA cvRisk will be restricted to 0 coefficient.
    # Otherwise algorithms with NA risk and all NA predictions can still receive
    # a positive coefficient. This does not bode well for this optimization
    # algorithm but we will handle anyway.
    if (anyNA(cvRisk)) {
      upper_bounds[is.na(cvRisk)] = 0
    }

    r <- nloptr::nloptr(x0 = rep(1 / ncol(modZ), ncol(modZ)),
            eval_f = obj_and_grad(Y, modlogitZ),
            lb = lower_bounds,
            ub = upper_bounds,
            eval_g_eq = function(beta) (sum(beta) - 1),
            eval_jac_g_eq = function(beta) rep(1, length(beta)),
            opts = list("algorithm" = "NLOPT_LD_SLSQP", "xtol_abs" = 1.0e-8))
    if (r$status < 1 || r$status > 4) {
      warning(r$message)
    }
    coef <- r$solution
    if (anyNA(coef)) {
      warning("Some algorithms have weights of NA, setting to 0.")
      coef[is.na(coef)] <- 0
    }
    # add in duplicated coefficients equal to 0 
    if(anyDupCols){
        ind <- c(seq_along(coef), dupCols - 0.5)
        coef <- c(coef,rep(0, length(dupCols)))
        coef <- coef[order(ind)]
    }
    # set very small coefficients to 0 and renormalize
    coef[coef < 1.0e-4] <- 0
    coef <- coef / sum(coef)
    out <- list(cvRisk = cvRisk, coef = coef, optimizer = r)
    return(out)
  }

  list(require = "nloptr",
       computeCoef = computeCoef,
       computePred = computePred)
}
        
method.AUC <- function(nlopt_method = NULL, optim_method = "L-BFGS-B",
                       bounds = c(0, Inf), normalize = TRUE) {
  # Contributed by Erin LeDell
  if (!is.null(nlopt_method) && !is.null(optim_method)) {
    stop("Please supply either a nlopt or optim method; one of these must be set to NULL.")
  }

  if (!is.null(optim_method)) {
    if (!(optim_method %in% c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"))) {
      stop("supplied 'optim_method' value not supported")
    }
    out <- list(
      require = 'cvAUC',
      # computeCoef is a function that returns a list with two elements:
      # 1) coef: the weights (coefficients) for each algorithm
      # 2) cvRisk: the V-fold CV risk for each algorithm
      computeCoef = function(Z, Y, libraryNames, obsWeights, control, verbose,
                             errorsInLibrary = NULL,
                             ...) {
        .cvRisk_AUC <- function(par, Z, Y, folds = NULL) {
          # Calculate cv Risk, which is 1 - cvAUC (rank loss);
          # This is the general loss function that gets fed into optim as the "fn" argument
          # par is the weight/coef vector for ensemble in Super Learner.
          predictions <- crossprod(t(Z[, par != 0, drop = FALSE]), par[par != 0])
          # Now calculate cv risk (this is what we want to minimize)
          # Might change this to AUC only since we are currently not using folds arg...
          cvRisk <- 1 - cvAUC::cvAUC(predictions = predictions, labels = Y, folds = folds)$cvAUC
          return(cvRisk)
        }

        coef_init <- rep(1 / ncol(Z), ncol(Z))
        names(coef_init) <- libraryNames

        # Don't need this currently.
        #lower_bounds = rep(bounds[1], ncol(Z))
        #upper_bounds = rep(bounds[2], ncol(Z))

        # Any algorithms with NA cvRisk will be restricted to 0 coefficient.
        # Otherwise algorithms with NA risk and all NA predictions can still receive
        # a positive coefficient. This does not bode well for this optimization
        # algorithm but we will handle anyway.
        if (sum(errorsInLibrary) > 0) {
          if (verbose) {
            cat("Removing failed learners:",
                paste(libraryNames[errorsInLibrary], collapse = ", "), "\n")
          }
          # Setting upper_bounds to 0 causes optim() to error out.
          # But this part isn't actually necessary.
          #upper_bounds[errorsInLibrary] = 0

          # Also update initial coefficients so that NA learners are set to 0.
          coef_init <- rep(1 / sum(!errorsInLibrary), ncol(Z))
          coef_init[errorsInLibrary] = 0
        }

        # optim function selects the value for par that minimizes .cvRisk_AUC (ie. rank loss)
        res <- optim(par = coef_init,
                     fn = .cvRisk_AUC,
                     Z = Z,
                     Y = Y,
                     folds = NULL,
                     method = optim_method,
                     lower = bounds[1],
                     upper = bounds[2])
        if (res$convergence != 0) {
          warning(paste("optim didn't converge when estimating the super learner coefficients, reason (see ?optim): ", res$convergence, " optim message: ", res$message))
        }
        coef <- res$par
        if (anyNA(coef)) {
          warning("Some algorithms have weights of NA, setting to 0.")
          coef[is.na(coef)] <- 0
        }
        if (!sum(coef) > 0) warning("All algorithms have zero weight", call. = FALSE)
        if (normalize) coef <- coef / sum(coef)

        auc <- apply(Z, 2, function(x) cvAUC::AUC(predictions = x, labels = Y))
        # If we update the getCoef function in SL to include 'folds' we can use the below auc instead
        # auc <- apply(Z, 2, function(x) cvAUC(x, labels=Y, folds=validRows)$cvAUC)
        cvRisk <- 1 - auc  # rank loss

        names(coef) <- libraryNames
        out <- list(cvRisk = cvRisk, coef = coef, optimizer = res)
        return(out)
      },
      # computePred is a function that takes the weights and the predicted values from each algorithm in the library and combines them based on the model to output the super learner predicted values
      computePred = function(predY, coef, control, ...) {
        if (sum(coef != 0) == 0) {
          warning("All metalearner coefficients are zero, all predictions will be 0")
        }
        out <- crossprod(t(predY[, coef != 0, drop = F]), coef[coef != 0])
        return(out)
      }
    )
#  } else if (length(nlopt_method) > 0) {
  } else if (!is.null(nlopt_method)) {
    nlopt_global <- c("NLOPT_GN_DIRECT",
                      "NLOPT_GN_DIRECT_L",
                      "NLOPT_GN_DIRECT_L_RAND",
                      "NLOPT_GN_DIRECT_NOSCAL",
                      "NLOPT_GN_DIRECT_L_NOSCAL",
                      "NLOPT_GN_DIRECT_L_RAND_NOSCAL",
                      "NLOPT_GN_ORIG_DIRECT",
                      "NLOPT_GN_ORIG_DIRECT_L",
                      "NLOPT_GN_CRS2_LM",
                      "NLOPT_GN_ISRES")
    nlopt_local <- c("NLOPT_LN_PRAXIS",
                     "NLOPT_LN_COBYLA",
                     "NLOPT_LN_NEWUOA_BOUND",
                     "NLOPT_LN_NELDERMEAD",
                     "NLOPT_LN_SBPLX",
                     "NLOPT_LN_BOBYQA")
    #if (length(intersect(nlopt_method, c(nlopt_global, nlopt_local))) == 0) {
    if (!(nlopt_method %in% c(nlopt_global, nlopt_local))) {
      stop("supplied 'nlopt_method' value not supported")
    }
    out <- list(
      require = c('cvAUC', 'nloptr'),
      # computeCoef is a function that returns a list with two elements:
      # 1) coef: the weights (coefficients) for each algorithm
      # 2) cvRisk: the V-fold CV risk for each algorithm
      computeCoef = function(Z, Y, libraryNames, obsWeights, control, verbose, ...) {
        .cvRisk_AUC <- function(par, Z, Y){
          # Calculate cv Risk, which is 1-cvAUC (rank loss);
          # This is the general loss function that gets fed into optim as the "fn" argument
          # par is the weight/coef vector for ensemble in Super Learner
          predictions <- crossprod(t(Z), par)  #cv predicted SL values
          # Now calculate cv risk (this is what we want to minimize)
          cvRisk <- 1 - cvAUC::cvAUC(predictions = predictions, labels = Y, folds = NULL)$cvAUC
          return(cvRisk)
        }
        coef_init <- rep(1/ncol(Z), ncol(Z))
        names(coef_init) <- libraryNames
        # nloptr function selects the value for par that minimizes .cvRisk_AUC (ie. rank loss)
        res <- nloptr::nloptr(x0 = coef_init,
                      eval_f = .cvRisk_AUC,
                      lb = rep(bounds[1], ncol(Z)),
                      ub = rep(bounds[2], ncol(Z)),
                      #eval_g_ineq = .constraint_ineq,
                      #eval_g_eq = .constraint_eq,
                      opts = list(algorithm = nlopt_method, xtol_rel = 1e-08),
                      Z = Z,
                      Y = Y)
        if (res$status < 1 || res$status > 4) {
      warning(res$message)
    }
        coef <- res$solution
        if (anyNA(coef)) {
          warning("Some algorithms have weights of NA, setting to 0.")
          coef[is.na(coef)] <- 0
        }
        if (!sum(coef) > 0) warning("All algorithms have zero weight", call. = FALSE)
        if (normalize) coef <- coef/sum(coef)
        auc <- apply(Z, 2, function(x) cvAUC::AUC(predictions = x, labels = Y))
        ## If we update the getCoef function in SL to include 'folds' we can use the below auc instead
        ## auc <- apply(Z, 2, function(x) cvAUC(x, labels=Y, folds=validRows)$cvAUC)
        cvRisk <- 1 - auc  # rank loss
        names(coef) <- libraryNames
        out <- list(cvRisk = cvRisk, coef = coef, optimizer = res)
        return(out)
      },
      # computePred is a function that takes the weights and the predicted values from each algorithm in the library and combines them based on the model to output the super learner predicted values
      computePred = function(predY, coef, control, ...) {
        out <- crossprod(t(predY), coef)
        return(out)
      }
    )
  } else {
    stop("Please supply an nlopt or optim method.")
  }
  invisible(out)
}

