\name{write.SL.template}
\alias{write.SL.template}
\alias{SL.template}
\alias{predict.SL.template}
% \alias{SL.bart}
% \alias{predict.SL.bart}
\alias{SL.bayesglm}
\alias{predict.SL.bayesglm}
\alias{SL.caret}
\alias{predict.SL.caret}
\alias{SL.caret.rpart}
%\alias{SL.cforest}
\alias{predict.SL.cforest}
% \alias{SL.DSA}
% \alias{predict.SL.DSA}
\alias{SL.earth}
\alias{predict.SL.earth}
\alias{SL.gam}
\alias{predict.SL.gam}
\alias{SL.gbm}
\alias{predict.SL.gbm}
\alias{SL.glm.interaction}
\alias{SL.ipredbagg}
\alias{predict.SL.ipredbagg}
\alias{SL.knn}
\alias{predict.SL.knn}
\alias{SL.loess}
\alias{predict.SL.loess}
\alias{SL.logreg}
\alias{predict.SL.logreg}
\alias{SL.mean}
\alias{predict.SL.mean}
\alias{SL.nnet}
\alias{predict.SL.nnet}
\alias{SL.polymars}
\alias{predict.SL.polymars}
\alias{SL.randomForest}
\alias{predict.SL.randomForest}
\alias{SL.rpart}
\alias{SL.rpartPrune}
\alias{predict.SL.rpart}
\alias{SL.step}
\alias{predict.SL.step}
\alias{SL.step.forward}
\alias{SL.step.interaction}
\alias{SL.stepAIC}
\alias{predict.SL.stepAIC}
\alias{SL.svm}
\alias{predict.SL.svm}
\alias{SL.ridge}
\alias{predict.SL.ridge}
\alias{SL.leekasso}
\alias{predict.SL.leekasso}
\alias{SL.nnls}
\alias{predict.SL.nnls}

\title{
Wrapper functions for prediction algorithms in SuperLearner
}
\description{
Template function for SuperLearner prediction wrappers and built in options.
}
\usage{
write.SL.template(file = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
  A connection, or a character string naming a file to print to. Passed to \code{\link{cat}}.
}
  \item{\dots}{
  Additional arguments passed to \code{\link{cat}}
}
}
\details{
Describe SL.* structure here
}
\value{
A list with two elements:
  \item{pred}{
The predicted values for the rows in \code{newX}.
}
  \item{fit}{
A list. Contains all objects necessary to get predictions for new observations from specific algorithm.
}
}

\author{ Eric C Polley \email{epolley@uchicago.edu} }


\seealso{
\code{\link{SuperLearner}}
}

\examples{
write.SL.template(file = '')
}
\keyword{utilities}
