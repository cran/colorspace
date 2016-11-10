\name{hex2RGB}
\alias{hex2RGB}

\title{Convert Hexadecimal Color Specifications To RGB Objects}

\description{
  This function takes a vector of strings of the form
  \code{"#RRGGBB"} (hexadecimal color descriptions) into
  RGB objects.
}

\usage{
hex2RGB(x, gamma = FALSE)
}

\arguments{
  \item{x}{a vector of hexadecimal color descriptions.}
  \item{gamma}{Whether to apply gamma-correction.}
}

\details{
  This function converts device dependent color descriptions of the
  form \code{"#RRGGBB"} into sRGB color descriptions
  (linearized if \code{gamma} is \code{TRUE}).
  The alpha channel will be ignored if given (\code{"#RRGGBBAA"}).
}

\value{
  An RGB object describing the colors.
}

\author{Ross Ihaka}

\seealso{
  \code{\link{hex}},
  \code{\link{RGB}},
  \code{\link{sRGB}},
  \code{\link{HSV}},
  \code{\link{XYZ}},
  \code{\link{polarLAB}},
  \code{\link{LUV}},
  \code{\link{polarLUV}}.
}

\examples{
hex2RGB(c("#FF0000","#00FF00", "#0000FF50"))
}

\keyword{color}
