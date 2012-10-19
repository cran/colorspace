\name{hex}
\alias{hex}
\title{Convert Colors To Hexadecimal Strings}
\description{
  This functions converts ``color'' objects into
  hexadecimal strings.
}
\usage{
hex(from, gamma = NULL, fixup = FALSE)
}
\arguments{
  \item{from}{The color object to be converted.}
  \item{gamma}{Deprecated.}
  \item{fixup}{Should the color be corrected to a valid
    RGB value before correction.  The default is to convert
    out-of-gamut colors to the string \code{"NA"}.}
}
\details{
  The color objects are first converted to sRGB color objects.
  They are then multiplied by 255 and rounded to obtain an
  integer value.  These values are then converted to hexadecimal
  strings of the form \code{"#RRGGBB"} and suitable for use
  as color descriptions for R graphics.  Out of gamut values
  are either corrected to valid RGB values by translating the
  the individual primary values so that they lie between 0 and 255.
}
\value{
  A vector of character strings.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Ross Ihaka}
%\note{ ~~further notes~~ }
\seealso{
  \code{\link{hex2RGB}},
  \code{\link{RGB}},
  \code{\link{sRGB}},
  \code{\link{HSV}},
  \code{\link{XYZ}},
  \code{\link{LAB}},
  \code{\link{polarLAB}},
  \code{\link{LUV}},
  \code{\link{polarLUV}}.
}

\examples{
hsv = HSV(seq(0, 360, length = 7)[-7], 1, 1)
hsv
hex(hsv)
barplot(rep(1,6), col = hex(hsv))
}

\keyword{color}
