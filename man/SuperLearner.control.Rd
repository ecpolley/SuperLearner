\name{SuperLearner.control}
\alias{SuperLearner.control}

\title{
Control parameters for the SuperLearner
}
\description{
Control parameters for the \code{SuperLearner}}
\usage{
SuperLearner.control(saveFitLibrary = TRUE, saveCVFitLibrary = FALSE, trimLogit = 0.001)
}

\arguments{
  \item{saveFitLibrary}{
Logical. Should the fit for each algorithm be saved in the output from \code{SuperLearner}.
}
  \item{saveCVFitLibrary}{
Logical. Should cross-validated fits for each algorithm be saved in the output from \code{SuperLearner}.
}
  \item{trimLogit}{
number between 0.0 and 0.5. What level to truncate the logit transformation to maintain a bounded loss function when using the NNloglik method.
}
}

\value{
A list containing the control parameters.
}
\keyword{utilities}

