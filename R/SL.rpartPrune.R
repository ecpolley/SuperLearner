SL.rpartPrune <- function (Y, X, newX, family, obsWeights, cp = 0.001, minsplit = 20, xval = 10, maxdepth = 20, minbucket = 5, ...) 
{
    .SL.require("rpart")
    if (family$family == "gaussian") {
        fit.rpart <- rpart::rpart(Y ~ ., data = data.frame(Y, X), control = rpart::rpart.control(cp = cp, minsplit = minsplit, xval = xval, maxdepth = maxdepth, minbucket = minbucket), method = "anova", weights = obsWeights)
		    CP <- fit.rpart$cptable[which.min(fit.rpart$cptable[, "xerror"]), "CP"]
		    fitPrune <- rpart::prune(fit.rpart, cp = CP)
        pred <- predict(fitPrune, newdata = newX)
    }
    if (family$family == "binomial") {
        fit.rpart <- rpart::rpart(Y ~ ., data = data.frame(Y, X), control = rpart::rpart.control(cp = cp, minsplit = minsplit, xval = xval, maxdepth = maxdepth, minbucket = minbucket), method = "class", weights = obsWeights)
		    CP <- fit.rpart$cptable[which.min(fit.rpart$cptable[, "xerror"]), "CP"]
		    fitPrune <- rpart::prune(fit.rpart, cp = CP)
        pred <- predict(fitPrune, newdata = newX)[, 2]
    }
    fit <- list(object = fitPrune, fit = fit.rpart, cp = CP)
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.rpart")
    return(out)
}