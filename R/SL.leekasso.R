SL.leekasso <- function (Y, X, newX, family, obsWeights, id, ...) 
{
	.SL.require("sva") # Bioconductor package, but really only need the f.pvalue function, might just replace it with internal function?
	N <- length(Y)
	mod <- cbind(rep.int(1, N), Y)
  mod0 <- cbind(rep.int(1, N))
  pValues <- sva::f.pvalue(t(X), mod, mod0)
  index <- which(rank(pValues) <= 10) # always 10!

  lm1 <- lm(Y ~ ., data = X[, index])
  pred <- predict.lm(lm1, newdata = newX[, index])
  # pred <- numeric()
  fit <- list(object = lm1, index = index)
  class(fit) <- c("SL.leekasso")
  out <- list(pred = pred, fit = fit)
  return(out)
}

predict.SL.leekasso <- function(object, newdata, ...){
  pred <- predict(object = object$object, newdata = newdata[, object$index], type = "response")
  pred
}
## 
## f.pvalue function from sva package:
# f.pvalue <- function (dat, mod, mod0) 
# {
#     n <- dim(dat)[2]
#     m <- dim(dat)[1]
#     df1 <- dim(mod)[2]
#     df0 <- dim(mod0)[2]
#     p <- rep(0, m)
#     Id <- diag(n)
#     resid <- dat %*% (Id - mod %*% solve(t(mod) %*% mod) %*% 
#         t(mod))
#     rss1 <- rowSums(resid * resid)
#     rm(resid)
#     resid0 <- dat %*% (Id - mod0 %*% solve(t(mod0) %*% mod0) %*% 
#         t(mod0))
#     rss0 <- rowSums(resid0 * resid0)
#     rm(resid0)
#     fstats <- ((rss0 - rss1)/(df1 - df0))/(rss1/(n - df1))
#     p <- 1 - pf(fstats, df1 = (df1 - df0), df2 = (n - df1))
#     return(p)
# }
# <environment: namespace:sva>