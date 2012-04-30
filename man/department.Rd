\name{department}
\alias{department}
\docType{data}
\title{Labov's (1966) Department Store Study Data}
\description{
  This is the data set from Labov's well known 1966 rapid and anonymous study
  of department stores in NYC.
}
\usage{data(department)}
\format{
  A data frame with 729 observations on the following 4 variables.
  \describe{
    \item{\code{r}}{A factor coding whether \dQuote{r} was pronounced or not. \code{0} indicates \dQuote{r} deletion, and \code{r} indications \dQuote{r} pronunciation.}
    \item{\code{store}}{A factor coding the department stores Labov Visited \code{Kleins} \code{Macys} \code{Saks}}
    \item{\code{emphasis}}{a factor with levels \code{emphatic} \code{normal}}
    \item{\code{word}}{a factor with levels \code{floor} \code{fourth}}
  }
}
\details{
  In this study, Labov was investigating how frequently New Yorkers would drop
  their r's in the phrase "fourth floor." He approached employees of three different
  department stores in NYC (Sak's, Macy's and S. Klein's) and asked them where an item
  was which had previously determined was on the foorth floor. After noting whether they
  had dropped the r in either, both, or neither word, he pretended like he hadn't
  understood, and asked them to repeat the word, noting whether they dropped the r
  in either, both, or neither word in this more emphatic pronunciation.
}
\source{
  Labov, William. (1966) \emph{The Social Stratification of English in New York City}.
  Cambridge: Cambridge University Press
}

\examples{
data(department)
}
\keyword{datasets}
