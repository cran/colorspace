\name{readRGB}
\alias{readRGB}
\title{Read RGB Color Descriptions}
\description{
  This function reads a set of RGB color descriptions (of the form
  written by \code{gcolorsel}) from a file and creates a color
  object containing the corresponding colors.
  
}
\usage{
readRGB(file = "", class = "RGB")
}
\arguments{
  \item{file}{The file containing the color descriptions.}
  \item{class}{The kind of color object to be returned.}
}
\details{
  The file is assumed to contain RGB color descriptions
  consisting of three integer values in the range from
  0 to 255 followed by a color name.
}
\value{
  An color object of the specified class containing the color
  descriptions.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Ross Ihaka.}
%\note{ ~~further notes~~ }
\seealso{
  \code{\link{writehex}},
  \code{\link{readhex}},
  \code{\link{hex2RGB}},
  \code{\link{RGB}},
  \code{\link{HSV}},
  \code{\link{XYZ}},
  \code{\link{LAB}},
  \code{\link{polarLAB}},
  \code{\link{LUV}},
  \code{\link{polarLUV}}.
}
\examples{
\dontrun{
rgb <- readRGB("pastel.rgb")
hsv <- readRGB("pastel.rgb", "HSV")
}
}
\keyword{color}
