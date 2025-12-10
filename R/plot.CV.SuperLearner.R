#' Graphical display of the V-fold CV risk estimates
#'
#' The function plots the V-fold cross-validated risk estimates for the super
#' learner, the discrete super learner and each algorithm in the library. By
#' default the estimates will be sorted and include an asymptotic 95%
#' confidence interval.
#'
#' See [summary.CV.SuperLearner()] for details on how the estimates are
#' computed
#'
#' @param x The output from \code{CV.SuperLearner()}.
#' @param package Either `"ggplot2"` or `"lattice"`. The package selected must be
#' available.
#' @param constant A numeric value. The confidence interval is defined as `p +/- constant * se`, where `p` is the point estimate and `se` is the standard error. The default is the quantile of the standard normal corresponding to a 95% CI.
#' @param sort Logical. Should the rows in the plot be sorted from the smallest
#' to the largest point estimate. If `FALSE`, then the order is super learner,
#' discrete super learner, then the estimators in \code{SL.library}.
#' @param \dots Additional arguments for \code{summary.CV.SuperLearner()}.
#'
#' @returns
#' Returns the plot (either a ggplot2 object (class \code{ggplot}) or a
#' lattice object (class \code{trellis}))
#'
#' @author Eric C Polley \email{epolley@@uchicago.edu}
#'
#' @seealso \code{\link{summary.CV.SuperLearner}} and
#' \code{\link{CV.SuperLearner}}
#'
#' @keywords plot

#' @exportS3Method plot CV.SuperLearner
plot.CV.SuperLearner <- function(x, package = "ggplot2", constant = qnorm(0.975), sort = TRUE, ...) {
  sumx <- summary(x, ...)
  # if(sort) sumx$Table$Algorithm <- stats:::reorder.default(sumx$Table$Algorithm, -sumx$Table$Ave)\
	if (sort) sumx$Table$Algorithm <- reorder(sumx$Table$Algorithm, -sumx$Table$Ave)
  Mean <- sumx$Table$Ave
  se <- sumx$Table$se
  Lower <- Mean - constant*se
  Upper <- Mean + constant*se
  # d <- data.frame(Y = Mean, X = sumx$Table$Algorithm, Lower = Lower, Upper = Upper)
  assign("d", data.frame(Y = Mean, X = sumx$Table$Algorithm, Lower = Lower, Upper = Upper))

  if(package == "lattice") {
    .SL.require("lattice")
    p <- lattice::dotplot(X ~ Y, data = d, xlim = c(min(d$Lower) - 0.02, max(d$Upper) + 0.02), xlab = "V-fold CV Risk Estimate", ylab = "Method", panel = function(x, y){
      lattice::panel.xyplot(x, y, pch = 16, cex = 1)
      lattice::panel.segments(d$Lower, y, d$Upper, y, lty = 1)
    })
  }
  if(package == "ggplot2") {
    .SL.require("ggplot2")
    p <- ggplot2::ggplot(d, ggplot2::aes_string(x = "X", y = "Y", ymin = "Lower", ymax = "Upper")) + ggplot2::geom_pointrange() + ggplot2::coord_flip() + ggplot2::ylab("V-fold CV Risk Estimate") + ggplot2::xlab("Method")
  }
  return(p)
}