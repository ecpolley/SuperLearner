\name{plot.CV.SuperLearner}
\alias{plot.CV.SuperLearner}

\title{
Graphical display of the V-fold CV risk estimates
}
\description{
The function plots the V-fold cross-validated risk estimates for the super learner, the discrete super learner and each algorithm in the library. By default the estimates will be sorted and include an asymptotic 95\% confidence interval.
}
\usage{
\method{plot}{CV.SuperLearner}(x, package = "ggplot2", constant = qnorm(0.975), sort = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The output from \code{CV.SuperLearner}.
}
  \item{package}{
Either "ggplot2" or "lattice". The package selected must be available.
}
  \item{constant}{
A numeric value. The confidence interval is defined as p +/- constant * se, where p is the point estimate and se is the standard error. The default is the quantile of the standard normal corresponding to a 95\% CI.
}
  \item{sort}{
Logical. Should the rows in the plot be sorted from the smallest to the largest point estimate. If FALSE, then the order is super learner, discrete super learner, then the estimators in \code{SL.library}.
}
  \item{\dots}{
Additional arguments for \code{summary.CV.SuperLearner}
}
}
\details{
see \link{summary.CV.SuperLearner} for details on how the estimates are computed
}
\value{
Returns the plot (either a ggplot2 object (class \code{ggplot}) or a lattice object (class \code{trellis}))
}

\author{ Eric C Polley \email{epolley@uchicago.edu} }

\seealso{
\code{\link{summary.CV.SuperLearner}} and \code{\link{CV.SuperLearner}}
}

\keyword{plot}

