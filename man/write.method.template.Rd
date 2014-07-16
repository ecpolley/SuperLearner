\name{write.method.template}
\alias{write.method.template}
\alias{method.template}
\alias{method.NNLS}
\alias{method.NNLS2}
\alias{method.NNloglik}
\alias{method.CC_LS}
\alias{method.CC_nloglik}
\alias{method.AUC}


\title{
Method to estimate the coefficients for the super learner
}
\description{
These functions contain the information on the loss function and the model to combine algorithms
}
\usage{
write.method.template(file = "", ...)

## a few built in options:
method.NNLS()
method.NNLS2()
method.NNloglik()
method.CC_LS()
method.CC_nloglik()
method.AUC(optim_method = "Nelder-Mead")
}

\arguments{
  \item{file}{
A connection, or a character string naming a file to print to. Passed to \code{\link{cat}}.
}
	\item{optim_method}{
Passed to the \code{optim} call method. See \code{\link{optim}} for details.
}
  \item{\dots}{
Additional arguments passed to \code{\link{cat}}.
}
}

\details{
A \code{SuperLearner} method must be a list (or a function to create a list) with exactly 3 elements. The 3 elements must be named \code{require}, \code{computeCoef} and \code{computePred}.
}

\value{
A list containing 3 elements:
  \item{require}{
A character vector listing any required packages. Use \code{NULL} if no additional packages are required
}
  \item{computeCoef}{
A function. The arguments are: \code{Z}, \code{Y}, \code{libraryNames}, \code{obsWeights}, \code{control}, \code{verbose}. The value is a list with two items: \code{cvRisk} and \code{coef}. This function computes the coefficients of the super learner. As the super learner minimizes the cross-validated risk, the loss function information is contained in this function as well as the model to combine the algorithms in \code{SL.library}.
}
  \item{computePred}{
A function. The arguments are: \code{predY}, \code{coef}, \code{control}. The value is a numeric vector with the super learner predicted values.
}
}

\author{ Eric C Polley \email{eric.polley@nih.gov} }
\seealso{
\code{\link{SuperLearner}}
}

\examples{
write.method.template(file = '')
}

\keyword{utilities}

