#' HCL (and HSV) Color Palettes Corresponding to Base R Palettes
#' 
#' Color palettes based on the HCL (and HSV) color space to replace
#' base R palettes.
#' 
#' Based on the general qualitative, sequential, and diverging \code{\link{hcl_palettes}}
#' within the colorspace package, convenience functions are provided as
#' alternatives to standard base R palettes (which are highly saturated
#' and too flashy).
#'
#' \code{rainbow_hcl} computes a rainbow of colors via \code{\link{qualitative_hcl}}
#' defined by different hues given a single value of each chroma and luminance.
#' It corresponds to \code{\link{rainbow}} which computes a rainbow in HSV
#' space.
#'
#' \code{heat_hcl} is an implementation of \code{\link[grDevices]{heat.colors}} in
#' HCL space based on a call to \code{\link{sequential_hcl}}. Similarly,
#' \code{terrain_hcl} palette also calls \code{sequential_hcl} with different
#' parameters, providing colors similar in spirit to \code{terrain.colors}
#' in HCL space.
#' 
#' \code{diverging_hsv} (and equivalently its alias \code{diverge_hsv})
#' provides an HSV-based version of \code{\link{diverging_hcl}}. Its purpose
#' is mainly didactic to show that HSV-based diverging palettes are less
#' appealing, more difficult to read and more flashy than HCL-based diverging
#' palettes. \code{diverging_hsv} is similar to \code{\link[grDevices]{cm.colors}}.
#' 
#' @param n the number of colors (\eqn{\ge 1}{>= 1}) to be in the palette.
#' @param c,c. chroma value in the HCL color description.
#' @param l luminance value in the HCL color description.
#' @param start the hue at which the rainbow begins.
#' @param end the hue at which the rainbow ends.
#' @param h hue value in the HCL or HSV color description, has to be in [0, 360]
#' for HCL and in [0, 1] for HSV colors.
#' @param s saturation value in the HSV color description.
#' @param v value value in the HSV color description.
#' @param power control parameter determining how chroma and luminance should
#' be increased (1 = linear, 2 = quadratic, etc.).
#' @param gamma Deprecated.
#' @param fixup logical. Should the color be corrected to a valid RGB value
#' before correction?
#' @param alpha numeric vector of values in the range \code{[0, 1]} for alpha
#' transparency channel (0 means transparent and 1 means opaque).
#' @param \dots Other arguments passed to \code{\link{hex}}.
#' @return A character vector with (s)RGB codings of the colors in the palette.
#' @seealso \code{\link[colorspace]{polarLUV}}, \code{\link[colorspace]{HSV}},
#' \code{\link[colorspace]{hex}}
#' @references Zeileis A, Hornik K, Murrell P (2009).  Escaping RGBland:
#' Selecting Colors for Statistical Graphics.  \emph{Computational Statistics &
#' Data Analysis}, \bold{53}, 3259--3270.
#' \doi{10.1016/j.csda.2008.11.033}
#' Preprint available from
#' \url{https://www.zeileis.org/papers/Zeileis+Hornik+Murrell-2009.pdf}.
#' 
#' Stauffer R, Mayr GJ, Dabernig M, Zeileis A (2015).  Somewhere over the
#' Rainbow: How to Make Effective Use of Colors in Meteorological
#' Visualizations.  \emph{Bulletin of the American Meteorological Society},
#' \bold{96}(2), 203--216.
#' \doi{10.1175/BAMS-D-13-00155.1}
#' @keywords color
#' @examples
#' ## convenience demo function
#' wheel <- function(col, radius = 1, ...)
#'   pie(rep(1, length(col)), col = col, radius = radius, ...) 
#' 
#' ## compare base and colorspace palettes
#' ## (in color and desaturated)
#' par(mar = rep(0, 4), mfrow = c(2, 2))
#' ## rainbow color wheel
#' wheel(rainbow_hcl(12))
#' wheel(rainbow(12))
#' wheel(desaturate(rainbow_hcl(12)))
#' wheel(desaturate(rainbow(12)))
#' 
#' ## diverging red-blue colors
#' swatchplot(
#'   diverging_hsv(7),
#'   desaturate(diverging_hsv(7)),
#'   diverging_hcl(7, c = 100, l = c(50, 90)),
#'   desaturate(diverging_hcl(7, c = 100, l = c(50, 90))),
#'   nrow = 2
#' )
#' 
#' ## diverging cyan-magenta colors
#' swatchplot(
#'   cm.colors(7),
#'   desaturate(cm.colors(7)),
#'   diverging_hcl(7, "Cyan-Magenta"), ## or, similarly: Tropic
#'   desaturate(diverging_hcl(7, "Cyan-Magenta")),
#'   nrow = 2
#' )
#' 
#' ## heat colors
#' swatchplot(
#'   heat.colors(12),
#'   desaturate(heat.colors(12)),
#'   heat_hcl(12),
#'   desaturate(heat_hcl(12)),
#'   nrow = 2
#' )
#' 
#' ## terrain colors
#' swatchplot(
#'   terrain.colors(12),
#'   desaturate(terrain.colors(12)),
#'   terrain_hcl(12),
#'   desaturate(terrain_hcl(12)),
#'   nrow = 2
#' )
#' @rdname rainbow_hcl

#' @export
rainbow_hcl <- function(n, c = 50, l = 70, start = 0, end = 360 * (n - 1)/n,
                        gamma = NULL, fixup = TRUE, alpha = 1, ...)
{
    if (!is.null(gamma)) warning("'gamma' is deprecated and has no effect")
    if(n < 1L) return(character(0L))
    substr(qualitative_hcl(n, h = c(start, end), c = c, l = l, fixup = fixup, alpha = alpha, ...),
        1L, if(missing(alpha)) 7L else 9L)
}

#' @rdname rainbow_hcl
#' @export
heat_hcl <- function(n, h = c(0, 90), c. = c(100, 30), l = c(50, 90),
                     power = c(1/5, 1), gamma = NULL, fixup = TRUE, alpha = 1, ...)
{
    if (!is.null(gamma)) warning("'gamma' is deprecated and has no effect")
    if(n < 1L) return(character(0L))
    substr(sequential_hcl(n, h = h, c = c., l = l, power = power, fixup = fixup, alpha = alpha, ...),
        1L, if(missing(alpha)) 7L else 9L)
}

#' @rdname rainbow_hcl
#' @export
terrain_hcl <- function(n, h = c(130, 0), c. = c(80, 0), l = c(60, 95),
                        power = c(1/10, 1), gamma = NULL, fixup = TRUE, alpha = 1, ...)
{
    if (!is.null(gamma)) warning("'gamma' is deprecated and has no effect")
    if(n < 1L) return(character(0L))
    substr(sequential_hcl(n, h = h, c = c., l = l, power = power, fixup = fixup, alpha = alpha, ...),
        1L, if(missing(alpha)) 7L else 9L)
}

#' @rdname rainbow_hcl
#' @export
diverging_hsv <- function(n, h = c(240, 0), s = 1, v = 1, power = 1,
                          gamma = NULL, fixup = TRUE, alpha = 1, ...)
{
    if (!is.null(gamma))
        warning("'gamma' is deprecated and has no effect")
    if(n < 1L) return(character(0L))
    h <- rep(h, length.out = 2L)
    s <- s[1L]
    v <- v[1L]
    power <- power[1L]
    rval <- seq(-s, s, length.out = n)
    rval <- hex(as(HSV(H = ifelse(rval > 0, h[2L], h[1L]),
                       S = abs(rval)^power, V = v, ...), "RGB"),
                fixup = fixup, ...)

    if(!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
	alpha <- format(as.hexmode(round(alpha * 255 + 0.0001)),
	                width = 2L, upper.case = TRUE)
        rval <- paste(rval, alpha, sep = "")
    }

    return(rval)
}

#' @rdname rainbow_hcl
#' @usage NULL
#' @export
diverge_hsv <- diverging_hsv


## Analogous to sp::bpy.colors
## (currently not exported, just for hclwizard)
bpy <- function(n) {
  i <- 0.05 + 0.9 * 0:(n-1)/(n-1)
  r <- -0.78125 + 3.125 * i
  g <- -0.84 + 2 * i
  b <- 1 + as.numeric(i > 0.3) + as.numeric(i > 0.92)
  b <- c(0, 1.84, -11.5)[b] + c(4, -2, 12.5)[b] * i
  hex(sRGB(
    pmax(0, pmin(1, r)),
    pmax(0, pmin(1, g)),
    pmax(0, pmin(1, b))
  ))
}
