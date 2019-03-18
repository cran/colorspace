#' Desaturate Colors by Chroma Removal in HCL Space
#' 
#' Transform a vector of given colors to the corresponding colors with chroma
#' reduced (by a tunable amount) in HCL space.
#' 
#' If input \code{col} is a vector given colors are first transformed to RGB
#' (either using \code{\link[colorspace]{hex2RGB}} or
#' \code{\link[grDevices]{col2rgb}}) and then to HCL
#' (\code{\link[colorspace]{polarLUV}}).  In HCL, chroma is reduced
#' and then the color is transformed back to a hexadecimal
#' string.
#' 
#' If input \code{col} is a matrix with three rows named \code{R}, \code{G}, and
#' \code{B} (top down) they are interpreted as Red-Green-Blue values within the
#' range \code{[0-255]}. The desaturation takes place in the HCL space as well.
#' Instead of an (s)RGB color vector a matrix of the same size as the input
#' \code{col} with desaturated Red-Green-Blue values will be returned.
#' This can be handy to avoid too many conversions.
#' 
#' @param col vector of R colors. Can be any of the three kinds of R colors, i.e.,
#' either a color name (an element of \code{\link[grDevices]{colors}}), a hexadecimal string
#' of the form \code{"#rrggbb"} or \code{"#rrggbbaa"} (see
#' \code{\link[grDevices]{rgb}}), or an integer \code{i} meaning
#' \code{palette()[i]}. Input \code{col} can also be a matrix with three
#' rows containing R/G/B (0-255) values, see details.
#' @param amount numeric specifying the amount of desaturation where \code{1}
#' corresponds to complete desaturation, \code{0} to no desaturation, and
#' values in between to partial desaturation.
#' @param ... additional arguments. If \code{severity} is specified it will
#' overrule the input argument \code{amount} (for convenience).
#' @return A character vector with (s)RGB codings of the colors in the palette
#' if input \code{col} is a vector. If input \code{col} is a matrix with R/G/B
#' values a matrix of the same form and size will be returned.
#' @seealso \code{\link[colorspace]{polarLUV}}, \code{\link[colorspace]{hex}}, \code{\link[colorspace]{lighten}}
#' @references Zeileis A, Fisher JC, Hornik K, Ihaka R, McWhite CD, Murrell P, Stauffer R, Wilke CO (2019).
#' \dQuote{ccolorspace: A Toolbox for Manipulating and Assessing Colors and Palettes.}
#' arXiv:1903.06490, arXiv.org E-Print Archive. \url{http://arxiv.org/abs/1903.06490}
#' @keywords color
#' @examples
#' ## rainbow of colors and their desaturated counterparts
#' rainbow_hcl(12)
#' desaturate(rainbow_hcl(12))
#' 
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
#' ## apply desaturation directly on RGB values
#' RGB <- t(hex2RGB(rainbow(3))@coords * 255)
#' desaturate(RGB)
#' @export desaturate
#' @importFrom grDevices col2rgb

desaturate <- function(col, amount = 1, ...) {

  ## If "severity" is given in the dots argument: use it
  ## as "amount" (convenience)
  args = as.list(match.call(expand.dots = TRUE))
  if ( ! is.null(args$severity) ) amount <- args$severity

  ## keep indizes of NA colors
  NAidx <- which(is.na(col))

  ## col has to be hex code, otherwise col2rgb is used
  matrix_input <- is.matrix(col)
  if(is.character(col) &&
    (all(substr(col, 1L, 1L) == "#") & all(nchar(col) %in% c(7L, 9L))))
  {
    ## extract alpha from hex (if any)
    alpha <- substr(col, 8L, 9L)
    ## retain only RGB in hex
    col <- substr(col, 1L, 7L)
    ## convert to colorspace::sRGB
    col <- hex2RGB(col)
  # Colors are a wide matrix with three columns containing
  # R, G, B (0-255). Rownames have to be set!
  } else if ( matrix_input ) {
    stopifnot( all(toupper(rownames(col)) == c("R","G","B")) )
    col <- sRGB(t(col[1L:3L, ])/255)
  } else {
    col <- col2rgb(col, alpha = TRUE)
    ## extract alpha values (if non-FF)
    alpha <- format(as.hexmode(col[4L, ]), width = 2L, upper.case = TRUE)
    alpha[alpha == "FF"] <- ""
    ## retain only RGB
    col <- sRGB(t(col[1L:3L, ])/255)
  }
  
  ## convert to HCL and fix-up extreme luminance cases
  col <- as(col, "polarLUV")
  col@coords[, 2L] <- (1 - amount) * col@coords[, 2L]

  ## fix-up extreme luminance cases
  col@coords[col@coords[, 1L] <= 0 | col@coords[, 1L] >= 100, 2L:3L] <- 0
  
  ## Return matrix if input 'col' was of type matrix
  if ( matrix_input ) return( t( as(col,"sRGB")@coords * 255) )
  
  ## convert back to hex and add alpha again (if any)
  col <- hex(col)
  col <- paste(col, alpha, sep = "")
  if(length(NAidx) > 0) col[NAidx] <- NA
  return(col)
}
