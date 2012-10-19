\name{RGB}
\alias{RGB}
\title{Create RGB Colors}
\description{
  This function creates colors of class RGB;
  a subclass of the virtual ``color'' class.
}
\usage{
RGB(R, G, B, names)
}
\arguments{
  \item{R,G,B}{these arguments give the red, green and blue
    intensities of the colors (the values should lie between 0 and 1).
    The values can be provided in separate \code{R}, \code{G} and
    \code{B} vectors or in a three-column matrix passed as \code{R}.
  } 
  \item{names}{A vector of names for the colors (by default the row
    names of \code{R} are used).}
}
\details{
  This function creates colors in the linearized sRGB color space (IEC
  standard 61966).
}
\value{
  An object of class ``RGB'' which inherits from class ``color.''
}
\references{\url{www.srgb.com}}
\author{Ross Ihaka}
%\note{ ~~further notes~~ }

\seealso{
  \code{\link{sRGB}},
  \code{\link{HSV}},
  \code{\link{XYZ}},
  \code{\link{LAB}},
  \code{\link{polarLAB}},
  \code{\link{LUV}},
  \code{\link{polarLUV}}.
}

\examples{
# Create a random set of colors
set.seed(1)
RGB(R = runif(20), G = runif(20), B = runif(20))
}
\keyword{color}
