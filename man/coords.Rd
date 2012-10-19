\name{coords}
\alias{coords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extract the numerical coordinates of a color}
\description{
  This function returns a matrix with three columns which give the
  coordinates of a color in its natural color space.
}
\usage{
coords(color)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{color}{A color.}
}
\value{
  A numeric matrix giving the coordinates of the color.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Ross Ihaka.}
%\note{ ~~further notes~~ }

\seealso{
  \code{\link{RGB}},
  \code{\link{XYZ}},
  \code{\link{LAB}},
  \code{\link{polarLAB}},
  \code{\link{LUV}},
  \code{\link{polarLUV}},
  \code{\link{mixcolor}}.
}
\examples{
x <- RGB(1, 0, 0)
coords(as(x, "HSV"))
}
\keyword{color}
