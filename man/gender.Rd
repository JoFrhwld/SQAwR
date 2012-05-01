\name{gender}
\alias{gender}
\docType{data}
\title{
 Human Development Data broken down by Gender
}
\description{
  2006 estimates of human development data from \acronym{UNESCO}
}
\usage{data(gender)}
\format{
  A data frame with 194 observations on the following 9 variables.
  \describe{\item{\code{Country}}{a factor with levels \code{Afghanistan} \code{Albania} \code{Algeria} \code{Andorra} \ldots}
    \item{\code{LifeExp_Female}}{Female life expectancy in years}
    \item{\code{LifeExp_Male}}{Male life expectancy in years}
    \item{\code{Literacy_Female}}{Female literacy rate}
    \item{\code{Literacy_Male}}{Male literacy rate}
    \item{\code{Edu_Female}}{Percent of enrolled female students of potential female students}
    \item{\code{Edu_Male}}{Percent of enrolled male students of potential male students}
    \item{\code{Income_Female}}{Female estimated earned income in USD}
    \item{\code{Income_Male}}{Male estimated earned income in USD}
  }
}

\source{
  \emph{Human Development Report 2006: Beyond scarcity: 
    Power, poverty and the global water crisis.} \acronym{UNESCO} 
    \url{http://hdr.undp.org/en/reports/global/hdr2006/}
}
\references{
  Rogers, S. \emph{The Sex War Numbers.} March 9, 2009. 
    \url{http://www.guardian.co.uk/news/datablog/2009/mar/10/gender-educationsgendergap}
}
\examples{
data(gender)
}
\keyword{datasets}
