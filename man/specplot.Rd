\name{specplot}
\alias{specplot}

\title{Color Spectrum Plot}

\description{
  Visualization of color palettes (given as hex codes) in
  RGB and/or HCL coordinates.
}

\usage{
  specplot(x, rgb = TRUE, hcl = TRUE, fix = TRUE, cex = 1,
    type = "l", lwd = 2 * cex, lty = 1, pch = NULL,
    legend = TRUE, palette = TRUE, plot = TRUE) 
}

\arguments{
   \item{x}{character vector containing color hex codes.}
   \item{rgb}{logical or color specification. Should the RGB spectrum
     be visualized? Can also be a vector of three colors for the R/G/B
     coordinates.}
   \item{hcl}{logical or color specification. Should the HCL spectrum
     be visualized? Can also be a vector of three colors for the H/C/L
     coordinates.}
   \item{fix}{logical. Should the hues be fixed to be on a smooth(er)
     curve? For details see below.}
   \item{cex}{numeric. Character extension for figure axes and labels.}
   \item{type, lwd, lty, pch}{plotting parameters passed to
     \code{\link[graphics]{lines}} for drawing the RGB and HCL coordinates,
     respectively. Can be vectors of length 3.}
   \item{legend}{logical. Should legends for the coordinates be plotted?}
   \item{palette}{logical. Should the given palette \code{x} be plotted?}
   \item{plot}{logical. Should the RGB and/or HCL coordinates be
     plotted?}
}

\details{
The function \code{specplot} transforms a given color palette in hex
codes into their RGB (\code{\link[colorspace]{sRGB}}) or
HCL (\code{\link[colorspace]{polarLUV}}) coordinates. As the hues for
low-chroma colors are not (or poorly) identified, by default a smoothing
is applied to the hues (\code{fix = TRUE}). Also, to avoid jumps from
0 to 360 or vice versa, the hue coordinates are shifted suitably.

By default (\code{plot = TRUE}) the resulting RGB and HCL coordinates
are visualized by simple line plots along with the color palette \code{x}
itself.
}

\value{
  \code{specplot} invisibly returns a list with components
  \item{RGB}{a matrix of sRGB coordinates,}
  \item{HCL}{a matrix of HCL coordinates,}
  \item{hex}{original color palette \code{x}.}
}

\author{Reto Stauffer, Achim Zeileis}

\references{
  Zeileis A, Hornik K, Murrell P (2009).
  Escaping RGBland: Selecting Colors for Statistical Graphics.
  \emph{Computational Statistics & Data Analysis}, \bold{53}, 3259--3270.
  \doi{10.1016/j.csda.2008.11.033}
  Preprint available from \url{https://eeecon.uibk.ac.at/~zeileis/papers/Zeileis+Hornik+Murrell-2009.pdf}.

  Stauffer R, Mayr GJ, Dabernig M, Zeileis A (2015).
  Somewhere over the Rainbow: How to Make Effective Use of Colors in Meteorological Visualizations.
  \emph{Bulletin of the American Meteorological Society}, \bold{96}(2), 203--216.
  \doi{10.1175/BAMS-D-13-00155.1}
}

\seealso{
\code{\link{rainbow_hcl}}
}

\examples{
## spectrum of the (in)famous RGB rainbow palette
specplot(rainbow(100))

## spectrum of HCL-based palettes: qualitative/sequential/diverging
specplot(rainbow_hcl(100))
specplot(sequential_hcl(100))
specplot(diverge_hcl(100))

## return computed RGB and HCL coordinates
res <- specplot(rainbow(10), plot = FALSE)
print(res)
}

\keyword{misc}
