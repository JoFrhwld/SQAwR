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
  A data frame with 1056 observations on the following 4 variables.
  \describe{
  \item{\code{Store}}{A factor coding the department store: \code{Saks},
      \code{Macys}, and \code{S.Klein}.}
  \item{\code{Repetition}}{A factor coding the repetition of \dQuote{fourth floor}:
    \code{casual} for the first time, \code{emphatic} for the repetition.}
  \item{\code{Word}}{A factor coding each word: \code{fourth} and \code{floor}.}
  \item{\code{r}}{A factor coding the pronunciation of \dQuote{r}, \code{del} for deleted, \code{ind} for indeterminite, and \code{pron} for pronounced. }
  }
}
\details{
  In this study, Labov was investigating how frequently New Yorkers would drop
  their r's in the phrase "fourth floor." He approached employees of three different
  department stores in NYC (Saks 5th Avenue, Macy's and S. Klein's) and asked them where an item
  was which had previously determined was on the foorth floor. After noting whether they
  had dropped the r in either, both, or neither word, he pretended like he hadn't
  understood, and asked them to repeat the word, noting whether they dropped the r
  in either, both, or neither word in this more emphatic pronunciation.
}
\source{
  Labov, William. (1966 [2006] ) \emph{The Social Stratification of English in New York City}.
  Table 3.3. p 49. Cambridge: Cambridge University Press
}

\examples{
data(department)
}
\keyword{datasets}
