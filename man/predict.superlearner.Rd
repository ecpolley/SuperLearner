\name{predict.SuperLearner}
\alias{predict.SuperLearner}
\title{Predict method for SuperLearner object}
\description{
  Obtains predictions on a new data set from a SuperLearner fit.  May require the original data if one of the library algorithms uses the original data in its predict method.
}
\usage{
\method{predict}{SuperLearner}(object, newdata, X = NULL, Y = NULL, onlySL = FALSE, ...)
}
\arguments{
  \item{object}{ Fitted object from \code{SuperLearner} }
  \item{newdata}{ New X values for prediction }
  \item{X}{ Original data set used to fit \code{object} }
  \item{Y}{ Original outcome used to fit \code{object} }
  \item{onlySL}{ Logical. If TRUE, only compute predictions for algorithms with non-zero coefficients in the super learner object. Default is FALSE (computes predictions for all algorithms in library).}
  \item{\dots}{ Additional arguments passed to the \code{predict.SL.*} functions }
}
\details{
  If \code{newdata} is omitted the predicted values from \code{object} are returned.  Each algorithm in the Super Learner library needs to have a corresponding prediction function with the ``predict.'' prefixed onto the algorithm name (e.g. \code{predict.SL.glm} for \code{SL.glm}).
}
\value{
	\item{pred}{ Predicted values from Super Learner fit}
	\item{library.predict}{ Predicted values for each algorithm in library}
}

\author{ Eric C Polley \email{eric.polley@nih.gov} }

\seealso{ \code{\link{SuperLearner}}}

\keyword{models}

