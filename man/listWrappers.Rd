\name{listWrappers}
\alias{listWrappers}

\title{
list all wrapper functions in SuperLearner
}
\description{
List all wrapper functions in \code{\link{SuperLearner}} package
}
\usage{
listWrappers(what = "both")
}

\arguments{
  \item{what}{
What list to return. Can be \code{both} for both prediction algorithms and screening algorithms, \code{SL} for the prediction algorithms, \code{screen} for the screening algorithms, \code{method} for the estimation method details, or anything else will return a list of all (exported) functions in the \code{SuperLearner} package. Additional wrapper functions are available at \url{https://github.com/ecpolley/SuperLearnerExtra}.
}
}

\value{
Invisible character vector with all exported functions in the SuperLearner package
}

\author{ Eric C Polley \email{epolley@uchicago.edu} }

\seealso{
\code{\link{SuperLearner}}
}
\examples{
listWrappers(what = "SL")
listWrappers(what = "screen")
}
\keyword{utilities}

