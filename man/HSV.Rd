\name{HSV}
\alias{HSV}
\title{Create HSV Colors}
\description{
  This function creates colors of class HSV;
  a subclass of the virtual ``color'' class.
}
\usage{
HSV(H, S, V, names)
}
\arguments{
  \item{H,S,V}{These arguments give the hue, saturation and value
    of the colors. The values can be provided in separate \code{H},
    \code{S} and \code{V} vectors or in a three-column matrix passed
    as \code{H}.
  }
  \item{names}{A vector of names for the colors (by default the row
    names of \code{H} are used).}
}
\details{
  This function creates colors in the HSV color space which corresponds
  to the standard sRGB color space (IEC standard 61966).
  The hues should lie between between 0 and 360, and the saturations and
  values should lie between 0 and 1.
}
\value{
  An object of class ``HSV'' which inherits from class ``color.''
}
\references{\url{www.srgb.com}}
\author{Ross Ihaka}
%\note{ ~~further notes~~ }

\seealso{
  \code{\link{RGB}},
  \code{\link{XYZ}},
  \code{\link{LAB}},
  \code{\link{polarLAB}},
  \code{\link{LUV}},
  \code{\link{polarLUV}}.
}

\examples{
# A rainbow of full-intensity hues
HSV(seq(0, 360, length=13)[-13], 1, 1)
}

\keyword{color}
