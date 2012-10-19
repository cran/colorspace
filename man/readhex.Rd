\name{readhex}
\alias{readhex}
\title{Read Hexadecimal Color Descriptions}
\description{
  This function reads a set of hexadecimal color
  descriptions from a file and creates a color
  object containing the corresponding colors.
}
\usage{
readhex(file = "", class = "RGB")
}
\arguments{
  \item{file}{The file containing the color descriptions.}
  \item{class}{The kind of color object to be returned.}
}
\details{
  The file is assumed to contain hexadecimal color descriptions
  of the form \code{#RRGGBB}.
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
  \code{\link{readRGB}},
  \code{\link{hex2RGB}},
  \code{\link{RGB}},
  \code{\link{HSV}},
  \code{\link{XYZ}},
  \code{\link{LAB}},
  \code{\link{polarLAB}},
  \code{\link{LUV}},
  \code{\link{polarLUV}},
}
\examples{
\dontrun{
rgb <- readhex("pastel.txt")
hsv <- readhex("pastel.txt", "HSV")
}
}
\keyword{color}
