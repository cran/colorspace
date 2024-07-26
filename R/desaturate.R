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
#' Similarly, \code{col} can be a formal \code{\link[colorspace]{color-class}} object, in which
#' case the desaturated colors are returned as a formal object of the same class as the input.
#' 
#' @param col vector of R colors. Can be any of the three kinds of R colors,
#' i.e., either a color name (an element of \code{\link[grDevices]{colors}}), a hexadecimal (hex)
#' string of the form \code{"#rrggbb"} or \code{"#rrggbbaa"} (see
#' \code{\link[grDevices]{rgb}}), or an integer \code{i} meaning
#' \code{palette()[i]}. Additionally, \code{col} can be
#' a formal \code{\link[colorspace]{color-class}} object or a matrix with three named
#' rows (or columns) containing R/G/B (0-255) values.
#' @param amount numeric specifying the amount of desaturation where \code{1}
#' corresponds to complete desaturation, \code{0} to no desaturation, and
#' values in between to partial desaturation.
#' @param ... additional arguments. If \code{severity} is specified it will
#' overrule the input argument \code{amount} (for convenience).
#' @return A color object as specified in the input \code{col} (hexadecimal string, RGB matrix,
#' or formal color class) with desaturated colors.
#' @seealso \code{\link[colorspace]{polarLUV}}, \code{\link[colorspace]{hex}}, \code{\link[colorspace]{lighten}}
#' @references Zeileis A, Fisher JC, Hornik K, Ihaka R, McWhite CD, Murrell P, Stauffer R, Wilke CO (2020).
#' \dQuote{colorspace: A Toolbox for Manipulating and Assessing Colors and Palettes.}
#' \emph{Journal of Statistical Software}, \bold{96}(1), 1--49. \doi{10.18637/jss.v096.i01}
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
#' ## apply desaturation directly on wide RGB matrix (with R/G/B channels in rows)
#' RGB <- diag(3) * 255
#' rownames(RGB) <- c("R", "G", "B")
#' desaturate(RGB)
#' @export desaturate
#' @importFrom grDevices col2rgb
#' @importFrom stats setNames

desaturate <- function(col, amount = 1, ...) {

  ## convenience: interpret 'severity' argument in dots as 'amount'
  args <- as.list(match.call(expand.dots = TRUE))
  if (!is.null(args$severity)) amount <- args$severity

  ## determine input type
  input_type <- if (inherits(col, "color")) {
    ## S4 colorspace class
    "colorspace"
  } else if (is.matrix(col)) {
    ## named RGB matrix (0-255)
    "matrix"
  } else if (is.character(col) && (all(substr(col, 1L, 1L) == "#") & all(nchar(col) %in% c(7L, 9L)))) {
    ## all hex
    "hex"
  } else {
    ## assume built-in colors
    "other"
  }

  ## convert input to color object (typically sRGB)
  if (input_type == "colorspace") {

    color_class <- class(col)

  } else if (input_type == "matrix") { 

    if (NROW(col) != 3L && NCOL(col) == 3L && all(toupper(colnames(col)) == c("R", "G", "B"))) {
      col <- t(col)
      transpose <- FALSE
    } else {
      transpose <- TRUE
    }
    stopifnot(all(toupper(rownames(col)) == c("R", "G", "B")))
    col <- sRGB(t(col[1L:3L, ])/255)

  } else if (input_type == "hex") {

    # keep indices of NA colors
    NAidx <- which(is.na(col))

    ## extract alpha (if any) and convert RGB to colorspace::sRGB
    alpha <- substr(col, 8L, 9L)
    col <- hex2RGB(setNames(substr(col, 1L, 7L), names(col)))

  } else {

    # keep indices of NA colors
    NAidx <- which(is.na(col))
    col <- grDevices::col2rgb(col, alpha = TRUE)
    ## extract alpha values (if non-FF)
    alpha <- format(as.hexmode(col[4L, ]), width = 2L, upper.case = TRUE)
    alpha[alpha == "FF"] <- ""
    ## retain only RGB
    col <- sRGB(t(col[1L:3L, ])/255)

  }
  
  ## convert to HCL and decrease chroma
  col <- as(col, "polarLUV")
  col@coords[, 2L] <- (1 - amount) * col@coords[, 2L]

  ## fix-up extreme luminance cases
  col@coords[col@coords[, 1L] <= 0 | col@coords[, 1L] >= 100, 2L:3L] <- 0

  ## convert back to input type
  if (input_type == "colorspace") {
    ## convert back to original class
    col <- as(col, color_class)
  } else if (input_type == "matrix") {
    ## convert back to matrix (either long or wide)
    RGB <- as(col, "sRGB")@coords * 255
    col <- if (transpose) t(RGB) else RGB
  } else {
    ## convert back to hex and add alpha again (if any)
    col <- hex(col)
    col[] <- paste0(col[], alpha)
    if (length(NAidx) > 0L) col[NAidx] <- NA
  }
  return(col)
}
