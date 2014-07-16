\name{SuperLearnerNews}
\alias{SuperLearnerNews}
\alias{SuperLearnerDocs}

\title{Show the NEWS file for the SuperLearner package}
\description{
Show the NEWS file of the SuperLearner package. The function is simply a wrapper for the \code{RShowDoc} function
}
\usage{
SuperLearnerNews(\dots)
SuperLearnerDocs(what = 'SuperLearnerR.pdf', \dots)
}

\arguments{
  \item{\dots}{additional arguments passed to \code{RShowDoc}}
  \item{what}{specify what document to open. Currently supports the NEWS file and the PDF files 'SuperLearner.pdf' and 'SuperLearnerR.pdf'.}
}
\value{
A invisible character string given the path to the SuperLearner NEWS file
}
\keyword{utilities}
