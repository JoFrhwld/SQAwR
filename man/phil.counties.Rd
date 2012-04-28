\name{phil.counties}
\alias{phil.counties}
\docType{data}
\title{
  Median Income of the Greater Philadelphia Area
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(phil.counties)}
\format{
  A data frame with 40 observations on the following 5 variables.
  \describe{
    \item{\code{County}}{a factor with levels \code{Bucks County} \code{Delaware County} \code{Montgomery County} \code{Philadelphia County}}
    \item{\code{Gender}}{a factor with levels \code{Female} \code{Male}}
    \item{\code{Income}}{a numeric vector}
    \item{\code{Level}}{a factor with levels \code{LessHigh} \code{High} \code{SomeAssoc} \code{Bachelors} \code{GradProf}}
    \item{\code{Years}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(phil.counties)
## maybe str(phil.counties) ; plot(phil.counties) ...
}
\keyword{datasets}
