\name{sRGB}
\alias{sRGB}
\title{Create sRGB Colors}
\description{
  This function creates colors of class sRGB;
  a subclass of the virtual ``color'' class.
}
\usage{
sRGB(R, G, B, names)
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
  This function creates colors in the standard sRGB color space (IEC
  standard 61966).
}
\value{
  An object of class ``sRGB'' which inherits from class ``color.''
}
\references{\url{www.srgb.com}}
\author{Ross Ihaka}
%\note{ ~~further notes~~ }

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
# Create a random set of colors
set.seed(1)
sRGB(R = runif(20), G = runif(20), B = runif(20))
}
\keyword{color}
