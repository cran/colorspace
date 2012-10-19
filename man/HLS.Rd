\name{HLS}
\alias{HLS}
\title{Create HLS Colors}
\description{
  This function creates colors of class HLS;
  a subclass of the virtual ``color'' class.
}
\usage{
HLS(H, L, S, names)
}
\arguments{
  \item{H,L,S}{These arguments give the hue, lightness, and saturation 
    of the colors. The values can be provided in separate \code{H},
    \code{L} and \code{S} vectors or in a three-column matrix passed
    as \code{H}.
  }
  \item{names}{A vector of names for the colors (by default the row
    names of \code{H} are used).}
}
\details{
  This function creates colors in the HLS color space which corresponds
  to the standard sRGB color space (IEC standard 61966).
  The hues should lie between between 0 and 360, and the lightness
  and saturations should lie between 0 and 1.
}
\value{
  An object of class ``HLS'' which inherits from class ``color.''
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
HLS(seq(0, 360, length=13)[-13], 0.5, 1)
}

\keyword{color}
