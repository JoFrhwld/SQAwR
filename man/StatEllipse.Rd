\name{StatEllipse}
\alias{StatEllipse}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(StatEllipse)}
\format{
  The format is:
proto object 
 $ calculate       :function (., data, scales, level = 0.75, segments = 51, ...)   
  ..- attr(*, "source")= chr [1:3] "function(., data, scales, level = 0.75, segments = 51,...){" ... 
 $ calculate_groups:function (., data, scales, ...)   
  ..- attr(*, "source")= chr [1:3] "function(., data, scales, ...){" ... 
 $ objname         : chr "ellipse" 
 $ default_geom    :function (.)   
  ..- attr(*, "source")= chr "function(.) GeomPath" 
 $ required_aes    : chr [1:2] "x" "y" 
 parent: proto object 
 .. parent: proto object 
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
data(StatEllipse)
## maybe str(StatEllipse) ; plot(StatEllipse) ...
}
\keyword{datasets}
