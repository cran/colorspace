#' W3C Contrast Ratio
#' 
#' Compute (and visualize) the contrast ratio of pairs of colors, as defined by the World Wide
#' Web Consortium (W3C).
#' 
#' The W3C Content Accessibility Guidelines (WCAG) recommend a contrast
#' ratio of at least 4.5 for the color of regular text on the background color, and
#' a ratio of at least 3 for large text. See \url{https://www.w3.org/TR/WCAG21/#contrast-minimum}.
#'
#' The contrast ratio is defined in \url{https://www.w3.org/TR/WCAG21/#dfn-contrast-ratio}
#' as \code{(L1 + 0.05) / (L2 + 0.05)} where \code{L1} and \code{L2} are the relative luminances
#' (see \url{https://www.w3.org/TR/WCAG21/#dfn-relative-luminance}) of the lighter and darker
#' colors, respectively. The relative luminances are weighted sums of scaled sRGB coordinates:
#' \code{0.2126 * R + 0.7152 * G + 0.0722 * B} where each of \code{R}, \code{G}, and \code{B}
#' is defined as \code{ifelse(RGB <= 0.03928, RGB/12.92, ((RGB + 0.055)/1.055)^2.4)} based on
#' the \code{RGB} coordinates between 0 and 1.
#' 
#' @param col,col2 vectors of any of the three kind of R colors, i.e., either a color
#' name (an element of \code{\link[grDevices]{colors}}), a hexadecimal string
#' of the form \code{"#rrggbb"} or \code{"#rrggbbaa"} (see
#' \code{\link[grDevices]{rgb}}), or an integer \code{i} meaning
#' \code{palette()[i]}. Both can be vectors and are recycled as necessary.
#' @param plot logical indicating whether the contrast ratios should also be
#' visualized by simple color swatches. Can also be a vector of length 2, indicating
#' whether the foreground color should be visualized on the background color and/or
#' the background color on the foreground color.
#' @param border logical or color specification for the borders around the color swatches (only
#' used if \code{plot = TRUE}). The default is \code{FALSE} which is equivalent to
#' \code{"transparent"}. If \code{TRUE} the border is drawn in the same color as the text in the
#' rectangle.
#' @param cex numeric. Size of the text in the color color swatches (only if \code{plot = TRUE}).
#' @param off numeric. Vertical offset between the different color swatches (only if \code{plot = TRUE}).
#' Can also be of length 2 giving both vertical and horizontal offsets, respectively.
#' @param mar numeric. Size of the margins around the color swatches (only if \code{plot = TRUE}).
#' @param digits numeric. Number of digits for the contrast ratios displayed in the color swatches
#' (only if \code{plot = TRUE})
#' @param \dots further arguments passed to the plot of the color swatches (only if \code{plot = TRUE}).
#'
#' @return A numeric vector with the contrast ratios is returned (invisibly, if \code{plot} is \code{TRUE}).
#' @seealso \code{\link[colorspace]{desaturate}}
#' @references W3C (2018). \dQuote{Web Content Accessibility Guidelines (WCAG) 2.1.}
#' \url{https://www.w3.org/TR/WCAG21/}
#' @keywords color
#' @examples
#' # check contrast ratio of default palette on white background
#' contrast_ratio(palette(), "white")
#'
#' # visualize contrast ratio of default palette on white and black background
#' contrast_ratio(palette(), "white", plot = TRUE)
#' contrast_ratio(palette()[-1], "black", plot = TRUE)
#' @export contrast_ratio
#' @importFrom grDevices rgb
#' @importFrom graphics par
#' @importFrom graphics rect
#' @importFrom graphics text

contrast_ratio <- function(col, col2 = "white",
  plot = FALSE, border = FALSE, cex = 2, off = 0.05, mar = rep(0.5, 4), digits = 2L, ...)
{
  ## https://www.w3.org/TR/WCAG21/#dfn-contrast-ratio
  if(length(col) < 1L || length(col2) < 1L) stop("both 'col' and 'col2' need to specify at least one color")

  ## suitably recycle colors if necessary
  n <- max(c(length(col), length(col2)))
  if(!(length(col) == n && length(col2) == n)) {
    col <- rep_len(col, n)
    col2 <- rep_len(col2, n)
  }
  
  ## compute contrast ratio
  ratio <- (relative_luminance(col) + 0.05)/(relative_luminance(col2) + 0.05)
  ratio[ratio < 1] <- 1/ratio[ratio < 1]

  ## optionally visualize
  plot <- rep_len(plot, 2L)
  if(any(plot)) {
    opar <- par(mar = mar)
    on.exit(par(opar))
    if(identical(border, FALSE)) border <- "transparent"
    if(length(off) == 1L) off <- c(off, 0)
    plot(0, 0, xlim = c(0, sum(plot)), ylim = c(0, n), type = "n", axes = FALSE, xlab = "", ylab = "", ...)
    if(plot[1L]) {
      rect(0L, 1L:n - 1L, 1L - off[2L], 1L:n - off[1L], col = col, border = if(isTRUE(border)) col2 else border)
      text((1 - off[2L])/2, 1L:n - (1 - (1 - off[1L])/2), format(round(ratio, digits = digits), nsmall = 2L), cex = cex, col = col2)
    }
    if(plot[2L]) {
      rect(0L + plot[1L], 0L:(n - 1L), 1L - off[2L] + plot[1L], 1L:n - off[1L], col = col2, border = if(isTRUE(border)) col else border)
      text((1 - off[2L])/2 + plot[1L], 1L:n - (1 - (1 - off[1L])/2), format(round(ratio, digits = digits), nsmall = 2L), cex = cex, col = col)
    }
    invisible(ratio)
  } else {
    return(ratio)
  }
}

relative_luminance <- function(col) {
  ## https://www.w3.org/TR/WCAG21/#dfn-relative-luminance
  rgb <- t(col2rgb(col))/255
  rgb[] <- ifelse(rgb <= 0.03928, rgb/12.92, ((rgb + 0.055)/1.055)^2.4)
  as.numeric(rgb %*% c(0.2126, 0.7152, 0.0722))
}
