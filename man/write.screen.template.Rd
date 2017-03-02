\name{write.screen.template}
\alias{write.screen.template}
\alias{screen.template}
\alias{All}
\alias{screen.randomForest}
\alias{screen.SIS}
\alias{screen.ttest}
\alias{screen.corP}
\alias{screen.corRank}
\alias{screen.glmnet}

\title{
screening algorithms for SuperLearner
}
\description{
Screening algorithms for \code{SuperLearner} to be used with \code{SL.library}.
}
\usage{
write.screen.template(file = "", ...)
}

\arguments{
  \item{file}{
  A connection, or a character string naming a file to print to. Passed to \code{\link{cat}}.
}
  \item{\dots}{
Additional arguments passed to \code{\link{cat}}
}
}

\details{
Explain structure of a screening algorithm here:

}
\value{
  \item{whichVariable}{
  A logical vector with the length equal to the number of columns in \code{X}.  TRUE indicates the variable (column of X) should be included.
}
}

\author{ Eric C Polley \email{polley.eric@mayo.edu} }

\seealso{
\code{\link{SuperLearner}}
}

\examples{
write.screen.template(file = '')
}
\keyword{utilities}
