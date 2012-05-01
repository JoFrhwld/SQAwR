\name{gender.pop}
\alias{gender.pop}
\docType{data}
\title{
  International Populations broken down by Gender
}
\description{
  2008 population estimates of world population broken down by gender.
}
\usage{data(gender.pop)}
\format{
  A data frame with 460 observations on the following 3 variables.
  \describe{
    \item{\code{Country}}{A factor with levels for each of the 230 countries with population data.}
    \item{\code{Gender}}{A factor with levels \code{Female} \code{Male}}
    \item{\code{Population}}{A numeric vector.}
  }
}
\source{
  UN Statistics Division, Department of Economic and Social Affairs. \emph{World Population Prospects: The 2008 Revision} \url{http://www.un.org/esa/population/publications/wpp2008/wpp2008_highlights.pdf}
}
\examples{
data(gender.pop)
}
\keyword{datasets}
