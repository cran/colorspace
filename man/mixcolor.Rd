\name{mixcolor}
\alias{mixcolor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute the convex combination of two colors}
\description{
  This function can be used to compute the result of color
  mixing (it assumes additive mixing).
}
\usage{
mixcolor(alpha, color1, color2, where = class(color1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alpha}{The mixed color is obtained by combining
    an amount \code{1-alpha} of \code{color1} with an amount
    \code{alpha} of \code{color2}.}
  \item{color1}{The first color.}
  \item{color2}{The second color.}
  \item{where}{The color space where the mixing is to take place.}
}
\value{
  The mixed color.  This is in the color space specified by \code{where}.
}
\author{Ross Ihaka}

\seealso{
   \code{\link{RGB}},
   \code{\link{HSV}},
   \code{\link{XYZ}},
   \code{\link{LAB}},
   \code{\link{polarLAB}},
   \code{\link{LUV}},
   \code{\link{polarLUV}}.
}

\examples{
mixcolor(0.5, RGB(1, 0, 0), RGB(0, 1, 0))
}
\keyword{color}
