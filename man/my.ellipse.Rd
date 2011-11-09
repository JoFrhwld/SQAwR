\name{my.ellipse}
\alias{my.ellipse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
my.ellipse(x, y, level = 0.75, segments = 51)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{level}{
%%     ~~Describe \code{level} here~~
}
  \item{segments}{
%%     ~~Describe \code{segments} here~~
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
function(x, y, level = 0.75, segments = 51){
	dfn <- 2
    dfd <- length(x) - 1
    if (dfd < 3){
    	ellipse <- rbind(c(NA,NA))	
    } else {
        require(MASS)
        v <- cov.trob(cbind(x, y))
        shape <- v$cov
        center <- v$center
        radius <- sqrt(dfn * qf(level, dfn, dfd))
        angles <- (0:segments) * 2 * pi/segments
    	unit.circle <- cbind(cos(angles), sin(angles))
	    ellipse <- t(center + radius * t(unit.circle \%*\% chol(shape)))
    }
    
    ellipse <- as.data.frame(ellipse)
    colnames(ellipse) <- c("x","y")
    return(ellipse)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
