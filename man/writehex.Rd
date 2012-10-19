\name{writehex}
\alias{writehex}
\title{Write Hexadecimal Color Descriptions}
\description{
  Given a color object, this function writes a file
  containing the hexadecimal representation of the
  colors in the object.
}
\usage{
writehex(x, file = "")
}
\arguments{
  \item{x}{a color object.}
  \item{file}{the name of the file to be written.}
}
\details{
  This function converts the given color object to RGB and
  then writes hexadecimal strings (of the form \code{#RRGGBB})
  representing the colors to the specified file.  
}
\value{
  The name of the file is returned as the value of the function.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Ross Ihaka}
%\note{ ~~further notes~~ }
\seealso{
  \code{\link{readhex}},
  \code{\link{readRGB}},
  \code{\link{hex2RGB}},
  \code{\link{RGB}},
  \code{\link{HSV}},
  \code{\link{XYZ}},
  \code{\link{LAB}},
  \code{\link{polarLAB}},
  \code{\link{LUV}},
  \code{\link{polarLUV}}.
}

\examples{
set.seed(1)
x <- RGB(runif(10), runif(10), runif(10))
writehex(x, "random.txt")
}
\keyword{color}
