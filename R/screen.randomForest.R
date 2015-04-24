screen.randomForest <- function (Y, X, family, nVar = 10, ntree = 1000, mtry = ifelse(family$family=="gaussian", floor(sqrt(ncol(X))), max(floor(ncol(X)/3), 1)), nodesize = ifelse(family$family=="gaussian", 5, 1), ...) 
{
  .SL.require('randomForest')
  if (family$family == "gaussian") {
	  rank.rf.fit <- randomForest::randomForest(Y ~ ., data = X, ntree = ntree, mtry = mtry, nodesize = nodesize, keep.forest = FALSE)
  }
  if (family$family == "binomial") {
    rank.rf.fit <- randomForest::randomForest(as.factor(Y) ~ ., data=X, ntree=ntree, mtry = mtry, nodesize = nodesize, keep.forest = FALSE)
  }
  whichVariable <- (rank(-rank.rf.fit$importance) <= nVar)
  return(whichVariable)
}