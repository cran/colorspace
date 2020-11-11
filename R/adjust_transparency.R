#' Adjust Transparency of Colors
#' 
#' Add, remove, or modify alpha transparency of a vector of colors.
#'
#' Alpha transparency is useful for making colors semi-transparent, e.g., for
#' overlaying different elements in graphics. An alpha value of 0 (or 00 in hex strings)
#' corresponds to fully transparent and an alpha value of 1 (or FF in hex strings)
#' corresponds to fully opaque. If a color hex string in R does not provide an explicit
#' alpha transparency, the color is assumed to be fully opaque.
#'
#' The \code{adjust_transparency} function can be used to adjust the alpha transparency
#' of a set of colors. It always returns a hex color specification. This hex color
#' can have the alpha transparency added/removed/modified depending on the
#' specification of \code{alpha}: \itemize{
#' \item \code{alpha = NULL}: Returns a hex vector with alpha transparency only if needed.
#'       Thus, it keeps the alpha transparency for the colors (if any) but only if
#'       different from opaque.
#' \item \code{alpha = TRUE}: Returns a hex vector with alpha transparency
#'       for all colors, using opaque (FF) as the default if missing.
#' \item \code{alpha = FALSE}: Returns a hex vector without alpha transparency for
#'       all colors (even if the original colors had non-opaque alpha).
#' \item \code{alpha} numeric: Returns a hex vector with alpha transparency for
#'       all colors set to the \code{alpha} argument (recycled if necessary).
#' }
#'
#' @param col vector of R colors. Can be any of the three kinds of R colors,
#' i.e., either a color name (an element of
#' \code{\link[grDevices]{colors}}), a hexadecimal (hex) string of the form
#' \code{"#rrggbb"} or \code{"#rrggbbaa"} (see \code{\link[grDevices]{rgb}}), or
#' an integer \code{i} meaning \code{palette()[i]}. Additionally, \code{col} can be
#' a formal \code{\link[colorspace]{color-class}} object or a matrix with three
#' rows containing R/G/B (0-255) values.
#' @param alpha either a new numeric alpha transparency value or logical (to add/remove alpha)
#' or \code{NULL}. See details.
#' @return A character vector with hexadecimal color strings with alpha transparency
#' corresponding to \code{alpha} argument.
#' @seealso \code{\link[grDevices]{rgb}}, \code{\link[colorspace]{desaturate}}, \code{\link[colorspace]{lighten}}
#' @references Zeileis A, Fisher JC, Hornik K, Ihaka R, McWhite CD, Murrell P, Stauffer R, Wilke CO (2020).
#' \dQuote{ccolorspace: A Toolbox for Manipulating and Assessing Colors and Palettes.}
#' \emph{Journal of Statistical Software}, \bold{96}(1), 1--49. \doi{10.18637/jss.v096.i01}
#' @keywords color
#' @examples
#' ## modify transparency of a color (in different formats)
#' adjust_transparency("black",   alpha = c(0, 0.5, 1)) ## name
#' adjust_transparency("#000000", alpha = c(0, 0.5, 1)) ## hex string
#' adjust_transparency(1,         alpha = c(0, 0.5, 1)) ## palette() integer
#' 
#' ## three shades of gray (in different formats:
#' ## name/opaque, hex/opaque, hex/semi-transparent)
#' x <- c("gray", "#BEBEBE", "#BEBEBE80")
#' 
#' ## adjust transparency
#' adjust_transparency(x, alpha = NULL)  ## only if necessary
#' adjust_transparency(x, alpha = TRUE)  ## add
#' adjust_transparency(x, alpha = FALSE) ## remove
#' adjust_transparency(x, alpha = 0.8)   ## modify
#' @export adjust_transparency
#' @importFrom grDevices rgb col2rgb

adjust_transparency <- function(col, alpha = TRUE) {

  ## alpha argument controls new alpha values
  new_alpha <- alpha
  num_to_hex <- function(alpha) {
    format(as.hexmode(round(alpha * 255 + 0.0001)), width = 2L, upper.case = TRUE)
  }

  ## support S4 color specifications as well (with default alpha = 1)
  if(inherits(col, "color")) col <- paste0(hex(col), "FF")

  ## keep indizes of NA colors
  NAidx <- which(is.na(col))
  n <- if(is.matrix(col) && is.numeric(col)) NCOL(col) else length(col)

  ## col has to be hex code, otherwise col2rgb is used
  if(is.character(col) &&
    (all(substr(col, 1L, 1L) == "#") & all(nchar(col) %in% c(7L, 9L))))
  {
    ## extract alpha from hex (if any)
    alpha <- substr(col, 8L, 9L)
    ## retain only RGB in hex
    col <- substr(col, 1L, 7L)
  } else {
    if(!(is.matrix(col) && is.numeric(col))) col <- col2rgb(col, alpha = TRUE)
    ## extract alpha values (if any)
    alpha <- if(NROW(col) > 3L) num_to_hex(col[4L, ]/255) else rep.int("FF", n)
    ## retain only RGB
    col <- rgb(col[1L, ], col[2L, ], col[3L, ], maxColorValue = 255)
  }

  ## adjust alpha transparency
  if(is.null(new_alpha)) {
    alpha[alpha == "FF"] <- ""
  } else if(identical(new_alpha, FALSE)) {
    alpha <- rep.int("", n)
  } else if(identical(new_alpha, TRUE)) {
    alpha[alpha == ""] <- "FF"
  } else {
    alpha <- num_to_hex(new_alpha)
    if(length(alpha) != n) {
      n <- max(n, length(alpha))
      col <- rep_len(col, n)
      alpha <- rep_len(alpha, n)
    }
  }
  
  ## add alpha again (if any) and manage NAs
  col <- paste(col, alpha, sep = "")
  if(length(NAidx) > 0) col[NAidx] <- NA

  return(col)
}
