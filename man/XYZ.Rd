\name{XYZ}
\alias{XYZ}
\title{Create XYZ Colors}
\description{
 This function creates colors of class XYZ;
  a subclass of the virtual ``color'' class.
}
\usage{
XYZ(X, Y, Z, names)
}
\arguments{
  \item{X,Y,Z}{these arguments give the X, Y and Z coordinates of
    the colors.  The values can be provided in separate \code{X},
    \code{Y} and \code{Z} vectors or in a three-column matrix
    passed as \code{X}.}
  \item{names}{A vector of names for the colors (by default the row
    names of \code{X} are used).}
}
\details{
  The X, Y and Z values are the levels of the CIE primaries.
  These are scaled so that the luminance of the display white-point
  is 100.  The white-point is taken to be D65, which means
  that its coordinates are  95.047, 100.000, 108.883.
}
\value{
  An object of class ``XYZ'' which inherits from class ``color.''
}
%\references{\url{}}
\author{Ross Ihaka}
%\note{ ~~further notes~~ }
\seealso{
  \code{\link{RGB}},
  \code{\link{HSV}},
  \code{\link{LAB}},
  \code{\link{polarLAB}},
  \code{\link{LUV}},
  \code{\link{polarLUV}}.
}

\examples{
## Generate white in XYZ space
XYZ(95.047, 100.000, 108.883)
}
\keyword{color}
