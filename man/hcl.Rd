\name{hcl}
\alias{hcl}
\title{Create R Colours from Hue, Chroma and Luminance}
\description{
  This function creates a vector of R colours with the
  the given hue, chroma and luminance values.
}
\usage{
hcl(h = 0, c = 35, l = 85, gamma = 2.2, fixup = TRUE)
}
\arguments{
  \item{h}{The hue of the color as an angle in the interval [0,360].}
  \item{c}{The chroma of the color.}
  \item{l}{The luminance of the colour}
  \item{gamma}{The gamma value for the device the colours are to
    be viewed on.}
  \item{fixup}{a logical value which indicates whether a ``close''
    color should be substituted when the specified colour is out
    of gammut.}
}
\details{
  This function is intended as an alternative to color specification
  with the R \code{hsv} function.  The use of this function is more
  firmly based in perceptual theory.
}
\value{
  A vector (of character strings) wich can be used as color
  specifications for other R functions.
}
\references{Ihaka, R. (2003).
  ``Colour for Presentation Graphics,''
  Proceedings of DSC-2003.
}
\author{Ross Ihaka}
\examples{
barplot(rep(1, 12), col = hcl(seq(0, 360, length = 13)[1:12]))
}
\keyword{color}
