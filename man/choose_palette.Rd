\name{choose_palette}
\alias{choose_palette}

\title{Graphical User Interface for Choosing HCL Color Palettes}

\description{
A graphical user interface (GUI) for viewing, manipulating,
and choosing HCL color palettes.
}

\usage{
choose_palette(pal = diverge_hcl, n = 7L, parent = NULL)
}

\arguments{
  \item{pal}{function; the initial palette, see \sQuote{Value} below.}
  \item{n}{integer; the initial number of colors in the palette.}
  \item{parent}{tkwin; the GUI parent window.}
}

\details{
Computes palettes based on the HCL (hue-chroma-luminance) color
model (as implemented by \code{\link{polarLUV}}). The GUI interfaces
the palette functions
\code{\link{rainbow_hcl}} for qualitative palettes,
\code{\link{sequential_hcl}} for sequential palettes with a single hue,
\code{\link{heat_hcl}} for sequential palettes with multiple hues, and
\code{\link{diverge_hcl}} for diverging palettes (composed from two single-hue
sequential palettes).

The GUI allows for interactive modification of the arguments of the respective
palette-generating functions, i.e.,
starting/ending hue (wavelength, type of color),
minimal/maximal chroma (colorfulness),
minimal maximal luminance (brightness, amount of gray), and
a power transformations that control how quickly/slowly chroma and/or
luminance are changed through the palette. Subsets of the parameters
may not be applicable depending on the type of palette chosen. See
\code{\link{rainbow_hcl}} and Zeileis et al. (2009) for a more detailed
explanation of the different arguments

Optionally, active palette can be illustrated by using a range of examples
such as a map, heatmap, scatter plot, perspective 3D surface etc.

To demonstrate different types of deficiencies, the active palette may
be desaturated (emulating printing on a grayscale printer) and, if the
\code{\link[dichromat]{dichromat}} package is available, collapsed to
emulate different types of color-blindness (without red-green or green-blue
contrasts).
}

\value{
Returns a palette-generating function with the selected arguments.
Thus, the returned function takes an integer argument and returns the corresponding
number of HCL colors by traversing HCL space through interpolation
of the specified hue/chroma/luminance/power values.
}

\author{Jason C. Fisher and Achim Zeileis}

\references{
  Zeileis A., Hornik K. and Murrell P. (2009),
  Escaping RGBland: Selecting Colors for Statistical Graphics.
  \emph{Computational Statistics & Data Analysis}, \bold{53}, 3259-3270.
  doi:10.1016/j.csda.2008.11.033
  Preprint available from \url{http://eeecon.uibk.ac.at/~zeileis/papers/Zeileis+Hornik+Murrell-2009.pdf}.
}

\seealso{
\code{\link{rainbow_hcl}}
}

\examples{
\donttest{pal <- choose_palette()
filled.contour(volcano, color.palette = pal, asp = 1)}
}

\keyword{misc}
