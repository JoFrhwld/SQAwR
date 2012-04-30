\name{phil.counties}
\alias{phil.counties}
\docType{data}
\title{
  Median Income of the Greater Philadelphia Area by Sex and Education
}
\description{
  Median earnings in the past 12 months (in 2010 inflation adjusted dollars) by sex and educational 
  attainment for the population 25 years and over for the greater Philadelpia area.
}
\usage{data(phil.counties)}
\format{
  A data frame with 40 observations on the following 4 variables.
  \describe{
    \item{\code{County}}{a factor with levels \code{Bucks County} \code{Delaware County} \code{Montgomery County} \code{Philadelphia County}}
    \item{\code{Gender}}{a factor with levels \code{Female} \code{Male}}
    \item{\code{Income}}{Median income in 2010 dollars}
    \item{\code{Level}}{a factor with levels \code{LessHigh} \code{High} \code{SomeAssoc} \code{Bachelors} \code{GradProf}}
  }
}

\source{
  U.S. Census Bureau, 2010 American Community Survey
}

\examples{
data(phil.counties)
}
\keyword{datasets}
