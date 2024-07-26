#' Adjust or Extract Transparency of Colors
#' 
#' Adjust (i.e., add, remove, or modify) or extract alpha transparency of a vector of colors.
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
#' The \code{extract_transparency} function can be used to extract the alpha transparency
#' from a set of colors. It allows to specify the \code{default} value - that should be used
#' for colors without an explicit alpha transparency (defaulting to fully opaque) - and
#' \code{mode} of the return value. This can either be numeric (in [0, 1]), integer
#' (0L, 1L, \dots, 255L), character (\dQuote{00}, \dQuote{01}, \dots, \dQuote{FF}),
#' or an object of class \code{\link[base]{hexmode}} (internally represented as integer
#' with printing as character). The \code{default} can use any of these modes as well
#' (independent of the output \code{mode}) or be \code{NA}.
#'
#' @param col vector of R colors. Can be any of the three kinds of R colors,
#' i.e., either a color name (an element of
#' \code{\link[grDevices]{colors}}), a hexadecimal (hex) string of the form
#' \code{"#rrggbb"} or \code{"#rrggbbaa"} (see \code{\link[grDevices]{rgb}}), or
#' an integer \code{i} meaning \code{palette()[i]}. Additionally, \code{col} can be
#' a formal \code{\link[colorspace]{color-class}} object or a matrix with three
#' rows containing R/G/B (0-255) values.
#' @param alpha either a new alpha transparency value or logical (to add/remove alpha)
#' or \code{NULL}. See details.
#' @param mode character specifying the output mode for the alpha transparency, can be
#' \code{"numeric"}, \code{"integer"}, \code{"character"} or \code{"hexmode"}. See details.
#' @param default vector of length 1 specifying the default alpha transparency that should
#' be returned for colors that do not specify any explicitly (defaulting to fully opaque).
#' Can either be numeric, integer, character, or hexmode.
#' @return For \code{adjust_transparency} character vector with hexadecimal color strings with alpha transparency
#' corresponding to \code{alpha} argument. For \code{extract_transparency} a vector of
#' alpha transparency values with the indicated \code{mode}.
#' @seealso \code{\link[grDevices]{rgb}}, \code{\link[colorspace]{desaturate}}, \code{\link[colorspace]{lighten}}
#' @references Zeileis A, Fisher JC, Hornik K, Ihaka R, McWhite CD, Murrell P, Stauffer R, Wilke CO (2020).
#' \dQuote{colorspace: A Toolbox for Manipulating and Assessing Colors and Palettes.}
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
#' 
#' ## extract transparency in different formats
#' extract_transparency(x, mode = "numeric") ## default
#' extract_transparency(x, mode = "integer")
#' extract_transparency(x, mode = "character")
#' extract_transparency(x, mode = "hexmode")
#'
#' ## extract transparency with different default values
#' extract_transparency(x, default = NA)
#' extract_transparency(x, default = 0.5)
#' extract_transparency(x, default = 128L)
#' extract_transparency(x, default = "80", mode = "integer")
#' @export adjust_transparency
#' @importFrom grDevices rgb col2rgb
#' @importFrom stats setNames

adjust_transparency <- function(col, alpha = TRUE) {

  ## alpha argument controls new alpha values
  new_alpha <- alpha
  if(inherits(new_alpha, "hexmode")) new_alpha <- unclass(new_alpha)

  ## support S4 color specifications as well (with default alpha = 1)
  if(inherits(col, "color")) {
    col <- hex(col)
    col[] <- paste0(col[], "FF")
  }

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
    col <- setNames(substr(col, 1L, 7L), names(col))
  } else {
    if(!(is.matrix(col) && is.numeric(col))) col <- col2rgb(col, alpha = TRUE)
    ## extract alpha values (if any)
    alpha <- if(NROW(col) > 3L) convert_transparency(col[4L, ]/255, mode = "character") else rep.int("FF", n)
    ## retain only RGB
    col <- rgb(t(col[1L:3L, , drop = FALSE]), maxColorValue = 255, names = colnames(col))
  }

  ## adjust alpha transparency
  if(is.null(new_alpha)) {
    alpha[alpha == "FF"] <- ""
  } else if(identical(new_alpha, FALSE)) {
    alpha <- rep.int("", n)
  } else if(identical(new_alpha, TRUE)) {
    alpha[alpha == ""] <- "FF"
  } else {
    alpha <- convert_transparency(new_alpha, mode = "character")
    if(length(alpha) != n) {
      n <- max(n, length(alpha))
      if(length(col) != n) col <- rep_len(col, n)
      if(length(alpha) != n) alpha <- rep_len(alpha, n)
    }
  }
  
  ## add alpha again (if any) and manage NAs
  col[] <- paste(col[], alpha, sep = "")
  if(length(NAidx) > 0) col[NAidx] <- NA

  return(col)
}

#' @rdname adjust_transparency
#' @export extract_transparency
extract_transparency <- function(col, mode = "numeric", default = 1) {

  ## handle mode of return value
  mode <- match.arg(mode, c("numeric", "double", "integer", "character", "hexmode"))
  if(mode == "double") mode <- "numeric"

  ## handle default
  if(length(default) > 1L) {
    warning("'default' should be of length 1, first element used")
    default <- default[1L]
  } else if(length(default) < 1L) {
    default <- NA
  }
  if(inherits(default, "hexmode")) default <- unclass(default)
  if(mode == "hexmode") {
    hexmode <- TRUE
    mode <- "integer"
  } else {
    hexmode <- FALSE
  }
  na <- switch(mode,
    "numeric" = NA_real_,
    "character" = NA_character_,
    NA_integer_)
  if(is.na(default)) {
    default <- na
  } else if(is.character(default) | is.numeric(default)) {
    default <- convert_transparency(default, mode = mode)
  } else {
    warning("unknown type of 'default' using NA instead")
    default <- na
  }

  ## for S4 color specifications or RGB matrices the default is used
  if(inherits(col, "color")) col <- coords(col)

  ## number of colors and position of NA colors
  if(is.matrix(col) && is.numeric(col)) {
    n <- NCOL(col)
    ina <- which(apply(is.na(col), 1L, any))
  } else {
    n <- length(col)
    ina <- which(is.na(col))
  }

  ## cases where alpha can be extracted
  ialpha <- if(is.character(col)) {
    which(substr(col, 1L, 1L) == "#" & nchar(col) == 9L)
  } else {
    integer(0)
  }

  ## set up return value
  alpha <- rep.int(default, n)
  if(length(ialpha) > 0L) alpha[ialpha] <- convert_transparency(substr(col[ialpha], 8L, 9L), mode = mode)
  alpha[ina] <- na
  if(hexmode) alpha <- structure(alpha, class = "hexmode")
  return(alpha)
}

## auxiliary function
convert_transparency <- function(x, mode = "numeric") {
  mode <- match.arg(mode, c("numeric", "integer", "character"))
  if(is.character(x)) {
    if(!all(toupper(x) %in% format(as.hexmode(0L:255L), width = 2L, upper.case = TRUE))) {
      stop("invalid character specification of alpha transparency, must be in 00, 01, ..., FF")
    }
    x <- switch(mode,
      "integer" = as.integer(as.hexmode(x)),
      "numeric" = as.integer(as.hexmode(x))/255,
      x)
  } else if(is.integer(x)) {
    if(!all(x %in% 0L:255L)) {
      stop("invalid integer specification of alpha transparency, must be in 0L, 1L, ..., 255L")
    }
    x <- switch(mode,
      "numeric" = x/255,
      "character" = format(as.hexmode(x), width = 2L, upper.case = TRUE),
      x)
  } else if(is.numeric(x)) {
    if(!all(x >= 0 & x <= 1)) {
      stop("invalid numeric specification of alpha transparency, must be in [0, 1]")
    }
    x <- switch(mode,
      "integer" = as.integer(round(x * 255 + 0.0001)),
      "character" = format(as.hexmode(round(x * 255 + 0.0001)), width = 2L, upper.case = TRUE),
      x)
  }
  return(x)
}

