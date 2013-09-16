\name{rainbow_hcl}
\alias{rainbow_hcl}
\alias{diverge_hcl}
\alias{diverge_hsv}
\alias{heat_hcl}
\alias{sequential_hcl}
\alias{terrain_hcl}
\title{HCL and HSV Color Palettes}
\encoding{UTF-8}
\description{
  Color palettes based on the HCL and HSV color spaces.
}
\usage{
rainbow_hcl(n, c = 50, l = 70, start = 0, end = 360*(n-1)/n,
  gamma = NULL, fixup = TRUE, alpha = 1, \dots)

sequential_hcl(n, h = 260, c. = c(80, 0), l = c(30, 90), power = 1.5,
  gamma = NULL, fixup = TRUE, alpha = 1, \dots)
heat_hcl(n, h = c(0, 90), c. = c(100, 30), l = c(50, 90), power = c(1/5, 1),
  gamma = NULL, fixup = TRUE, alpha = 1, \dots)
terrain_hcl(n, h = c(130, 0), c. = c(80, 0), l = c(60, 95), power = c(1/10, 1),
  gamma = NULL, fixup = TRUE, alpha = 1, \dots)

diverge_hcl(n, h = c(260, 0), c = 80, l = c(30, 90), power = 1.5,
  gamma = NULL, fixup = TRUE, alpha = 1, \dots)
diverge_hsv(n, h = c(240, 0), s = 1, v = 1, power = 1,
  gamma = NULL, fixup = TRUE, alpha = 1, \dots)
}
\arguments{
  \item{n}{the number of colors (\eqn{\ge 1}{>= 1}) to be in the palette.}
  \item{c, c.}{chroma value in the HCL color description.}
  \item{l}{luminance value in the HCL color description.}
  \item{start}{the hue at which the rainbow begins.}
  \item{end}{the hue at which the rainbow ends.}
  \item{h}{hue value in the HCL or HSV color description, has to be
    in [0, 360] for HCL and in [0, 1] for HSV colors.}
  \item{s}{saturation value in the HSV color description.}
  \item{v}{value value in the HSV color description.}
  \item{power}{control parameter determining how chroma
    and luminance should be increased (1 = linear, 2 = quadratic, etc.).}
  \item{gamma}{Deprecated.}
  \item{fixup}{logical. Should the color be corrected to a valid RGB
    value before correction?}
  \item{alpha}{numeric vector of values in the range \code{[0, 1]} for
    alpha transparency channel (0 means transparent and 1 means opaque).}
  \item{\dots}{Other arguments passed to \code{\link{hex}}.}
}
\details{
  All functions compute palettes based on either the HCL (\code{\link{polarLUV}})
  or the HSV (\code{\link{HSV}}) color space.
  
  \code{rainbow_hcl} computes a rainbow of colors (qualitative palette)
  defined by different hues given a single value of each chroma and luminance.
  It corresponds to \code{\link{rainbow}} which computes a rainbow in
  HSV space.

  \code{sequential_hcl} gives a sequential palette starting at the full
  color HCL(\code{h}, \code{c[1]}, \code{l[1]}) through to a light color
  HCL(\code{h}, \code{c[2]}, \code{l[2]}) by interpolation.
  
  \code{diverge_hcl} and \code{diverge_hsv}, compute 
  a set of colors diverging from a neutral center (gray or white, without
  color) to two different extreme colors (blue and red by default). This
  is similar to \code{\link{cm.colors}}. For the diverging HSV colors,
  two hues \code{h} are needed, a maximal saturation \code{s} and a
  fixed value \code{v}. The saturation is then varied to obtain the
  diverging colors. For the diverging HCL colors, again two hues \code{h}
  are needed, a maximal chroma \code{c} and two luminances \code{l}.
  The colors are then created by an interpolation between the 
  full color HCL(\code{h[1]}, \code{c}, \code{l[1]}), a neutral color HCL(\code{h}, 0, \code{l[2]})
  and the other full color HCL(\code{h[2]}, \code{c}, \code{l[1]}).

  The palette \code{heat_hcl} gives an implementation of \code{\link{heat.colors}}
  in HCL space. By default, it goes from a red to a yellow hue, while
  simultaneously going to lighter colors (i.e., increasing luminance)
  and reducing the amount of color (i.e., decreasing chroma).
  The \code{terrain_hcl} palette simply calls \code{heat_hcl} with different
  parameters, providing colors similar in spirit to \code{terrain.colors}.
  The lighter colors are not strictly HCL colors, though.
}

\references{
  Zeileis A., Hornik K. and Murrell P. (2009),
  Escaping RGBland: Selecting Colors for Statistical Graphics.
  \emph{Computational Statistics & Data Analysis}, \bold{53}, 3259-3270.
  doi:10.1016/j.csda.2008.11.033
  Preprint available from \url{http://eeecon.uibk.ac.at/~zeileis/papers/Zeileis+Hornik+Murrell-2009.pdf}.
}

\value{
  A character vector with (s)RGB codings of the colors in the palette.
}

\author{
  Achim Zeileis \email{Achim.Zeileis@R-project.org}
}
\seealso{
  \code{\link[colorspace]{polarLUV}},
  \code{\link[colorspace]{HSV}},
  \code{\link[colorspace]{hex}}
}
\examples{
## convenience demo functions
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col = col, radius = radius, ...) 

pal <- function(col, border = "light gray")
{
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

## qualitative palette
wheel(rainbow_hcl(12))

## a few useful diverging HCL palettes
par(mar = rep(0, 4), mfrow = c(4, 1))
pal(diverge_hcl(7))
pal(diverge_hcl(7, h = c(246, 40), c = 96, l = c(65, 90)))
pal(diverge_hcl(7, h = c(130, 43), c = 100, l = c(70, 90)))
pal(diverge_hcl(7, h = c(180, 70), c = 70, l = c(90, 95)))
pal(diverge_hcl(7, h = c(180, 330), c = 59, l = c(75, 95)))
pal(diverge_hcl(7, h = c(128, 330), c = 98, l = c(65, 90)))
pal(diverge_hcl(7, h = c(255, 330), l = c(40, 90)))
pal(diverge_hcl(7, c = 100, l = c(50, 90), power = 1))

## sequential palettes
pal(sequential_hcl(12))
pal(heat_hcl(12, h = c(0, -100), l = c(75, 40), c = c(40, 80), power = 1))
pal(terrain_hcl(12, c = c(65, 0), l = c(45, 95), power = c(1/3, 1.5)))
pal(heat_hcl(12, c = c(80, 30), l = c(30, 90), power = c(1/5, 1.5)))

## compare base and colorspace palettes
## (in color and desaturated)
par(mar = rep(0, 4), mfrow = c(2, 2))
## rainbow color wheel
wheel(rainbow_hcl(12))
wheel(rainbow(12))
wheel(desaturate(rainbow_hcl(12)))
wheel(desaturate(rainbow(12)))

## diverging red-blue colors
pal(diverge_hsv(7))
pal(diverge_hcl(7, c = 100, l = c(50, 90)))
pal(desaturate(diverge_hsv(7)))
pal(desaturate(diverge_hcl(7, c = 100, l = c(50, 90))))

## diverging cyan-magenta colors
pal(cm.colors(7))
pal(diverge_hcl(7, h = c(180, 330), c = 59, l = c(75, 95)))
pal(desaturate(cm.colors(7)))
pal(desaturate(diverge_hcl(7, h = c(180, 330), c = 59, l = c(75, 95))))

## heat colors
pal(heat.colors(12))
pal(heat_hcl(12))
pal(desaturate(heat.colors(12)))
pal(desaturate(heat_hcl(12)))

## terrain colors
pal(terrain.colors(12))
pal(terrain_hcl(12))
pal(desaturate(terrain.colors(12)))
pal(desaturate(terrain_hcl(12)))
}
\keyword{color}
