\name{trimLogit}
\alias{trimLogit}

\title{
truncated-probabilities logit transformation 
}
\description{
computes the logit transformation on the truncated probabilities
}
\usage{
trimLogit(x, trim = 1e-05)
}

\arguments{
  \item{x}{
vector of probabilities.
}
  \item{trim}{
value to truncate probabilities at. Currently symmetric truncation (trim and 1-trim).
}
}

\value{
logit transformed values
}


\examples{
x <- c(0.00000001, 0.0001, 0.001, 0.01, 0.1, 0.3, 0.7, 0.9, 0.99, 
  0.999, 0.9999, 0.99999999)
trimLogit(x, trim = 0.001)
data.frame(Prob = x, Logit = qlogis(x), trimLogit = trimLogit(x, 0.001))
}

\keyword{models}