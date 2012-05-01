\name{rdevel}
\alias{rdevel}
\docType{data}
\title{
  Contributed Packages to \acronym{CRAN} over Time
}
\description{
  The number of contributed packages to \acronym{CRAN} over time.
}
\usage{data(rdevel)}
\format{
  A data frame with 20 observations on the following 3 variables.
  \describe{
    \item{\code{vers}}{R version}
    \item{\code{count}}{Number of contributed packages}
    \item{\code{date}}{Date of version / package count}
  }
}

\source{
  \url{blog.revolutionanalytics.com/2010/01/rs-exponential-package- growth-ctd.html}
}

\examples{
data(rdevel)
}
\keyword{datasets}
