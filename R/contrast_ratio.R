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
#' For use in the next major revision of the WCAG a new advanced perceptual contrast algorithm
#' (APCA) has been proposed by Somers (2022), see also Muth (2022) for more background and details.
#' APCA is still under development, here version 0.98G-4g is implemented. Unlike the standard WCAG
#' algorithm, APCA takes into account which color is the text and which is the background. Hence
#' for the APCA algorithm a matrix with normal and reverse polarity is returned. An absolute
#' value of 45 is "sort of" like a WCAG ratio of 3, 60 is "sort of" like 4.5.
#'
#' @param col,col2 vectors of any of the three kind of R colors, i.e., either a color
#' name (an element of \code{\link[grDevices]{colors}}), a hexadecimal string
#' of the form \code{"#rrggbb"} or \code{"#rrggbbaa"} (see
#' \code{\link[grDevices]{rgb}}), or an integer \code{i} meaning
#' \code{palette()[i]}. Both can be vectors and are recycled as necessary.
#' @param algorithm character specifying whether the established standard \code{"WCAG"} 2.1 algorithm
#' should be used or the improved \code{"APCA"} 0.98G-4g algorithm, still under development.
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
#'
#' Somers A (2022). \dQuote{Advanced Perceptual Contrast Algorithm.}
#' \url{https://github.com/Myndex/SAPC-APCA}
#'
#' Muth LC (2022). \dQuote{It's Time for a More Sophisticated Color Contrast Check for Data Visualizations.}
#' Datawrapper Blog. \url{https://blog.datawrapper.de/color-contrast-check-data-vis-wcag-apca/}
#' @keywords color
#' @examples
#' # check contrast ratio of default palette on white background
#' contrast_ratio(palette(), "white")
#'
#' # visualize contrast ratio of default palette on white and black background
#' contrast_ratio(palette(), "white", plot = TRUE)
#' contrast_ratio(palette()[-1], "black", plot = TRUE)
#'
#' # APCA algorithm
#' contrast_ratio(palette(), "white", algorithm = "APCA")
#' contrast_ratio(palette(), "white", algorithm = "APCA", plot = TRUE, digits = 0)
#' @export contrast_ratio
#' @importFrom grDevices rgb
#' @importFrom graphics par
#' @importFrom graphics rect
#' @importFrom graphics text

contrast_ratio <- function(col, col2 = "white", algorithm = c("WCAG", "APCA"),
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
  
  ## compute relative luminances
  algorithm <- match.arg(algorithm, c("WCAG", "APCA"))
  lum  <- relative_luminance(col, algorithm = algorithm)
  lum2 <- relative_luminance(col2, algorithm = algorithm)
  
  ## compute contrast ratio
  if(algorithm == "WCAG") {
    ratio <- (lum + 0.05)/(lum2 + 0.05)
    ratio[ratio < 1] <- 1/ratio[ratio < 1]
  }
  if(algorithm == "APCA") {
    apca <- function(txt, bg) {
      ## for normal polarity (bg luminance > txt luminance)
      ratio <- (bg^0.56 - txt^0.57) * 1.14
      ratio <- ifelse(ratio < 0.1, 0, ratio - 0.027)
      ## for reverse polarity
      rev <- bg <= txt
      ratio[rev] <- (bg[rev]^0.65 - txt[rev]^0.62) * 1.14
      ratio[rev] <- ifelse(ratio[rev] > -0.1, 0, ratio[rev] + 0.027)
      ## zero for essentially identical luminances
      ratio[abs(bg - txt) < 0.0005] <- 0
      ratio <- ratio * 100
      return(ratio)    
    }
    ratio <- cbind("normal" = apca(lum, lum2), "reverse" = apca(lum2, lum))
  }

  ## optionally visualize
  plot <- rep_len(plot, 2L)
  if(any(plot)) {
    opar <- par(mar = mar)
    on.exit(par(opar))
    if(identical(border, FALSE)) border <- "transparent"
    if(length(off) == 1L) off <- c(off, 0)
    plot(0, 0, xlim = c(0, sum(plot)), ylim = c(0, n), type = "n", axes = FALSE, xlab = "", ylab = "", ...)
    if(plot[1L]) {
      r <- format(round(if(algorithm == "WCAG") ratio else ratio[, 1L], digits = digits), nsmall = digits)
      rect(0L, n:1L - 1L, 1L - off[2L], n:1L - off[1L], col = col2, border = if(isTRUE(border)) col else border)
      text((1 - off[2L])/2, n:1L - (1 - (1 - off[1L])/2), r, cex = cex, col = col)
    }
    if(plot[2L]) {
      r <- format(round(if(algorithm == "WCAG") ratio else ratio[, 2L], digits = digits), nsmall = digits)
      rect(0L + plot[1L], n:1L - 1L, 1L - off[2L] + plot[1L], n:1L - off[1L], col = col, border = if(isTRUE(border)) col2 else border)
      text((1 - off[2L])/2 + plot[1L], n:1L - (1 - (1 - off[1L])/2), r, cex = cex, col = col2)
    }
    invisible(ratio)
  } else {
    return(ratio)
  }
}

relative_luminance <- function(col, algorithm = c("WCAG", "APCA")) {
  ## raw sRGB coordinates
  rgb <- t(col2rgb(col))/255

  ## type of algorithm
  algorithm <- match.arg(algorithm, c("WCAG", "APCA"))
  
  ## sRGB coefficients
  coef <- round(c(0.2126478133913640, 0.7151791475336150, 0.0721730390750208),
    digits = switch(algorithm, "WCAG" = 4, "APCA" = 16))

  ## relative luminance is essentially (rgb^2.4) %*% coef
  ## but with slightly different adjustments for WCAG vs. APCA
  if(algorithm == "WCAG") {
    ## https://www.w3.org/TR/WCAG21/#dfn-relative-luminance
    rgb[] <- ifelse(rgb <= 0.03928, rgb/12.92, ((rgb + 0.055)/1.055)^2.4)
    rlum <- as.numeric(rgb %*% coef)
  } else {
    ## https://github.com/Myndex/SAPC-APCA
    rgb[] <- rgb^2.4
    rlum <- as.numeric(rgb %*% coef)
    ## black soft clamp
    clamp <- rlum <= 0.022
    rlum[clamp] <- rlum[clamp] + (0.022 - rlum[clamp])^1.414
  }

  return(rlum)
}
