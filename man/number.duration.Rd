\name{number.duration}
\alias{number.duration}
\docType{data}
\title{
  The Duration of Read Numeral Sequences
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(number.duration)}
\format{
  A data frame with 651 observations on the following 6 variables.
  \describe{
    \item{\code{Begin}}{Numeral start in seconds from the beginning of the recording.}
    \item{\code{End}}{Numeral end in seconds from the beginning of the recording}
    \item{\code{Duration}}{Duration of the numeral in seconds.}
    \item{\code{Label}}{a factor with levels \code{zero} \code{one} \code{two} \code{three} \code{four} \code{five} \code{six} \code{seven} \code{eight} \code{nine}}
    \item{\code{Num}}{ID for the entire numeral sequence}
    \item{\code{Pos}}{Position of the numeral in the sequence}
  }
}
\details{
  I read 93 randomly generated numeral sequences formatted like telephone numbers. 
}

\examples{
data(number.duration)
}
\keyword{datasets}
