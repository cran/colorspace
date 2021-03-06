% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colorspace.R
\name{HLS}
\alias{HLS}
\title{Create HLS Colors}
\usage{
HLS(H, L, S, names)
}
\arguments{
\item{H, L, S}{These arguments give the hue, lightness, and saturation of the
colors. The values can be provided in separate \code{H}, \code{L} and
\code{S} vectors or in a three-column matrix passed as \code{H}.}

\item{names}{A vector of names for the colors (by default the row names of
\code{H} are used).}
}
\value{
An object of class \code{HLS} which inherits from class \code{color}.
}
\description{
This function creates colors of class HLS; a subclass of the virtual
\code{\link{color-class}} class.
}
\details{
This function creates colors in an HLS color space.  The hues should
lie between between 0 and 360, and the lightness and saturations
should lie between 0 and 1.

HLS is a relative color space; it is a transformation of an RGB color
space.  Conversion of HLS colors to any other color space must first
involve a conversion to a specific RGB color space, for example the
standard \code{\link{sRGB}} color space (IEC standard 61966).
}
\examples{
# A rainbow of full-intensity hues
HLS(seq(0, 360, length.out = 13)[-13], 0.5, 1)
}
\seealso{
\code{\link{sRGB}}, \code{\link{RGB}}, \code{\link{XYZ}}, \code{\link{LAB}},
\code{\link{polarLAB}}, \code{\link{LUV}}, \code{\link{polarLUV}}.
}
\author{
Ross Ihaka
}
\keyword{color}
