\name{CVFolds}
\alias{CVFolds}

\title{
Generate list of row numbers for each fold in the cross-validation
}
\description{
Generate list of row numbers for each fold in the cross-validation. \code{CVFolds} is used in the \code{SuperLearner} to create the cross-validation splits.
}
\usage{
CVFolds(N, id, Y, cvControl)
}
\arguments{
  \item{N}{
Sample size
}
  \item{id}{
Optional cluster id variable. If present, all observations in the same cluster will always be in the same split.
}
  \item{Y}{
outcome
}
  \item{cvControl}{
Control parameters for the cross-validation step. See \code{\link{SuperLearner.CV.control}} for details.
  }
}

\value{
\item{validRows}{
A list of length V where each element in the list is a vector with the row numbers of the corresponding validation sample.
}
}

\author{ Eric C Polley \email{polley.eric@mayo.edu} }

\keyword{utilities}
