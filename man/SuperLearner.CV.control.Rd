\name{SuperLearner.CV.control}
\alias{SuperLearner.CV.control}

\title{
Control parameters for the cross validation steps in \code{SuperLearner}
}
\description{
Control parameters for the cross validation steps in \code{SuperLearner}
}
\usage{
SuperLearner.CV.control(V = 10L, stratifyCV = FALSE, shuffle = TRUE, 
  validRows = NULL)
}

\arguments{
  \item{V}{
Integer. Number of splits for the V-fold cross-validation step. The default is 10. In most cases, between 10 and 20 splits works well.
}
  \item{stratifyCV}{
Logical. Should the data splits be stratified by a binary response? Attempts to maintain the same ratio in each training and validation sample.
}
  \item{shuffle}{
Logical. Should the rows of \code{X} be shuffled before creating the splits.
}
  \item{validRows}{
A List. Use this to pass pre-specified rows for the sample splits. The length of the list should be \code{V} and each entry in the list should contain a vector with the row numbers of the corresponding validation sample.
}
}

\value{
A list containing the control parameters
}

\keyword{utilities}