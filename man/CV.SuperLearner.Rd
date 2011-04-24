\name{CV.SuperLearner}
\alias{CV.SuperLearner}
\alias{print.CV.SuperLearner}
\alias{coef.CV.SuperLearner}

\title{
Wrapper to get V-fold cross-validated risk estimate for super learner
}
\description{
Wrapper to get V-fold cross-validated risk estimate for super learner
}
\usage{
CV.SuperLearner(Y, X, V = 20, family = gaussian(), SL.library, method = "method.NNLS", id = NULL, verbose = FALSE, control = list(saveFitLibrary = FALSE), cvControl = list(), obsWeights = NULL, saveAll = TRUE)
}

\arguments{
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{V}{
%%     ~~Describe \code{V} here~~
}
  \item{family}{
%%     ~~Describe \code{family} here~~
}
  \item{SL.library}{
%%     ~~Describe \code{SL.library} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{id}{
%%     ~~Describe \code{id} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{control}{
%%     ~~Describe \code{control} here~~
}
  \item{cvControl}{
%%     ~~Describe \code{cvControl} here~~
}
  \item{obsWeights}{
%%     ~~Describe \code{obsWeights} here~~
}
  \item{saveAll}{
%%     ~~Describe \code{saveAll} here~~
}
}

\details{
Why use CV.SuperLearner?
}
\value{
A List.
}


\author{ Eric C Polley \email{eric.polley@nih.gov} }

\seealso{
\code{\link{SuperLearner}}
}

\examples{
\dontrun{
  CV.SuperLearner()
}
}

\keyword{models}
