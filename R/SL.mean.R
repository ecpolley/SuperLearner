SL.mean <- function (Y, X, newX, family, obsWeights, id, ...) 
{
  meanY <- weighted.mean(Y, w = obsWeights)
  pred <- rep.int(meanY, times = nrow(newX))
  fit <- list(object = meanY)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.mean")
  return(out)
}

predict.SL.mean <- function (object, newdata, family, X = NULL, Y = NULL, ...) 
{
    pred <- rep.int(object$object, times = nrow(newdata))
    return(pred)
}
