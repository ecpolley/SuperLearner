plot.CV.SuperLearner <- function(x, package = "ggplot2", constant = qnorm(0.975), sort = TRUE, ...) {
  sumx <- summary(x, ...)
  # if(sort) sumx$Table$Algorithm <- stats:::reorder.default(sumx$Table$Algorithm, -sumx$Table$Ave)\
	if(sort) sumx$Table$Algorithm <- reorder(sumx$Table$Algorithm, -sumx$Table$Ave)
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