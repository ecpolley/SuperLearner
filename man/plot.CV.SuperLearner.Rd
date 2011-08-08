\name{plot.CV.SuperLearner}
\alias{plot.CV.SuperLearner}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.CV.SuperLearner(x, package = "ggplot2", constant = qnorm(0.975), sort = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{package}{
%%     ~~Describe \code{package} here~~
}
  \item{constant}{
%%     ~~Describe \code{constant} here~~
}
  \item{sort}{
%%     ~~Describe \code{sort} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, package = "ggplot2", constant = qnorm(0.975), sort = TRUE, ...) {
  sumx <- summary(x, ...)
  if(sort) sumx$Table$Algorithm <- reorder(sumx$Table$Algorithm, -sumx$Table$Ave)
  Mean <- sumx$Table$Ave
  se <- sumx$Table$se
  Lower <- Mean - constant*se
  Upper <- Mean + constant*se
  d <- data.frame(Y = Mean, X = sumx$Table$Algorithm, Lower = Lower, Upper = Upper)
  
  if(package == "lattice") {
    require(lattice)
    p <- dotplot(X ~ Y, data = d, xlim = c(min(d$Lower) - 0.02, max(d$Upper) + 0.02), xlab = "V-fold CV Risk Estimate", ylab = "Method", panel = function(x, y){
      panel.xyplot(x, y, pch = 16, cex = 1)
      panel.segments(d$Lower, y, d$Upper, y, lty = 1)
    })
  } 
  if(package == "ggplot2") {
    require(ggplot2)
    p <- ggplot(d, aes(x = X, y = Y, ymin = Lower, ymax = Upper)) + geom_pointrange() + coord_flip() + ylab("V-fold CV Risk Estimate") + xlab("Method")
  }
  return(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
