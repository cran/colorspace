\name{hex2RGB}
\alias{hex2RGB}
\title{Convert Hexadecimal Color Specifications To RGB Objects}
\description{
  This function takes a vector of strings of the form
  \code{"#RRGGBB"} (hexdecimal color descriptions) into
  RGB objects.
}
\usage{
hex2RGB(x, gamma = 2.2)
}
\arguments{
  \item{x}{a vector of hexdecimal color descriptions.}
  \item{gamma}{the display gamma value.  Passing a value
    of \code{NA} results in no gamma correction being
    applied.}
}
\details{
  This function converts device dependent color descriptions of the
  form \code{"#RRGGBB"} into device independent sRGB colour descriptions.
}
\value{
  An RGB object describing the colours.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Ross Ihaka}
%\note{ ~~further notes~~ }
\seealso{
  \code{\link{hex}},
  \code{\link{RGB}},
  \code{\link{HSV}},
  \code{\link{XYZ}},
  \code{\link{polarLAB}},
  \code{\link{LUV}},
  \code{\link{polarLUV}}.
}

\examples{
rgb = hex2RGB(c("#FF0000","#00FF00", "#0000FF"))
}
\keyword{color}
