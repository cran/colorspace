#' Simulate Color Vision Deficiency
#'
#' Transformation of R colors by simulating color vision deficiencies,
#' based on a CVD transform matrix.
#'
#' Using the physiologically-based model for simulating color vision deficiency (CVD)
#' of Machado et al. (2009), different kinds of limitations can be
#' emulated: deuteranope (green cone cells defective), protanope (red cone cells defective),
#' and tritanope (blue cone cells defective).
#' The workhorse function to do so is \code{simulate_cvd} which can take any vector
#' of valid R colors and transform them according to a certain CVD transformation
#' matrix (see \code{\link{cvd}}) and transformation equation.
#'
#' The functions \code{deutan}, \code{protan}, and \code{tritan} are the high-level functions for
#' simulating the corresponding kind of colorblindness with a given severity.
#' Internally, they all call \code{simulate_cvd} along with a (possibly interpolated)
#' version of the matrices from \code{\link{cvd}}. Matrix interpolation can be carried out with
#' the function \code{interpolate_cvd_transform} (see examples).
#'
#' If input \code{col} is a matrix with three rows named \code{R}, \code{G}, and
#' \code{B} (top down) they are interpreted as Red-Green-Blue values within the
#' range \code{[0-255]}. Then the CVD transformation is applied directly to these
#' coordinates avoiding any further conversions.
#'
#' Finally, if \code{col} is a formal \code{\link[colorspace]{color-class}} object, then its
#' coordinates are transformed to (s)RGB coordinates, as described above, and returned as a formal
#' object of the same class after the color vision deficiency simulation.
#'
#' Up to version 2.0-3 of the package, the CVD transformations had been applied
#' directly to the gamma-corrected sRGB coordinates (corresponding to the hex coordinates
#' of the colors), following the illustrations of Machado et al. (2009). However,
#' the paper implicitly relies on a linear RGB space (see page 1294, column 1) where their
#' linear matrix transformations for simulating color vision deficiencies are applied.
#' Therefore, starting from version 2.1-0 of the package, a new argument \code{linear = TRUE}
#' has been added that first maps the provided colors to linearized RGB coordinates, applies
#' the color vision deficiency transformation, and then maps back to gamma-corrected sRGB
#' coordinates. Optionally, \code{linear = FALSE} can be used to restore the behavior
#' from previous versions. For most colors the difference between the two strategies is
#' negligible but for some highly-saturated colors it becomes more noticable, e.g., for
#' red, purple, or orange.
#'
#' @param col vector of R colors. Can be any of the three kinds of R colors,
#' i.e., either a color name (an element of \code{\link[grDevices]{colors}}), a hexadecimal (hex)
#' string of the form \code{"#rrggbb"} or \code{"#rrggbbaa"} (see \code{\link[grDevices]{rgb}}), or
#' an integer \code{i} meaning \code{palette()[i]}. Additionally, \code{col} can be
#' a formal \code{\link[colorspace]{color-class}} object or a matrix with three named
#' rows (or columns) containing R/G/B (0-255) values.
#' @param severity numeric. Severity of the color vision defect, a number between 0 and 1.
#' @param cvd_transform numeric 3x3 matrix, specifying the color vision deficiency transform matrix.
#' @param linear logical. Should the color vision deficiency transformation be applied to the
#' linearized RGB coordinates (default)? If \code{FALSE}, the transformation is applied to the
#' gamma-corrected sRGB coordinates (which was the default up to version 2.0-3 of the package).
#' @param cvd list of cvd transformation matrices. See \code{\link{cvd}} for available options.
#'
#' @return A color object as specified in the input \code{col} (hexadecimal string, RGB matrix,
#' or formal color class) with simulated color vision deficiency.
#'
#' @references Machado GM, Oliveira MM, Fernandes LAF (2009).
#'   \dQuote{A Physiologically-Based Model for Simulation of Color Vision Deficiency.}
#'   \emph{IEEE Transactions on Visualization and Computer Graphics}. \bold{15}(6), 1291--1298.
#'   \doi{10.1109/TVCG.2009.113}
#'   Online version with supplements at
#'   \url{http://www.inf.ufrgs.br/~oliveira/pubs_files/CVD_Simulation/CVD_Simulation.html}.
#'
#' Zeileis A, Fisher JC, Hornik K, Ihaka R, McWhite CD, Murrell P, Stauffer R, Wilke CO (2020).
#'   \dQuote{colorspace: A Toolbox for Manipulating and Assessing Colors and Palettes.}
#'   \emph{Journal of Statistical Software}, \bold{96}(1), 1--49.
#'   \doi{10.18637/jss.v096.i01}
#' @keywords colors cvd colorblind
#' @seealso \code{\link{cvd}}
#' @export
#' @examples
#' # simulate color-vision deficiency by calling `simulate_cvd` with specified matrix
#' simulate_cvd(c("#005000", "blue", "#00BB00"), tritanomaly_cvd["6"][[1]])
#' 
#' # simulate color-vision deficiency by calling the shortcut high-level function
#' tritan(c("#005000", "blue", "#00BB00"), severity = 0.6)
#' 
#' # simulate color-vision deficiency by calling `simulate_cvd` with interpolated cvd matrix
#' simulate_cvd(c("#005000", "blue", "#00BB00"),
#'              interpolate_cvd_transform(tritanomaly_cvd, severity = 0.6))
#'
#' # apply CVD directly on wide RGB matrix (with R/G/B channels in rows)
#' RGB <- diag(3) * 255
#' rownames(RGB) <- c("R", "G", "B")
#' deutan(RGB)
#' 
#' @importFrom grDevices col2rgb
simulate_cvd <- function(col, cvd_transform, linear = TRUE) {
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

  ## indexes of missing values (if hex)
  NAidx <- NULL

  ## convert input to wide RGB matrix (0-255)
  if (input_type == "colorspace") {
  
    color_class <- class(col)
    col <- t(coords(as(col, if(linear) "RGB" else "sRGB"))) * 255
  
  } else if (input_type == "matrix") { 

    if(NROW(col) != 3L && NCOL(col) == 3L && all(toupper(colnames(col)) == c("R", "G", "B"))) {
      col <- t(col)
      transpose <- TRUE
    } else {
      transpose <- FALSE
    }
    stopifnot(all(toupper(rownames(col)) == c("R", "G", "B")))

  } else if (input_type == "hex") {

    # Save transparency value for later
    alpha <- substr(col, 8L, 9L)
    # keep indizes of NA colors
    NAidx <- which(is.na(col))
    col <- substr(col, 1L, 7L)
    col <- grDevices::col2rgb(col)

  } else {

    # keep indizes of NA colors
    NAidx <- which(is.na(col))
    col <- grDevices::col2rgb(col, alpha = TRUE)
    ## extract alpha values (if non-FF)
    alpha <- format(as.hexmode(col[4L, ]), width = 2L, upper.case = TRUE)
    alpha[alpha == "FF"] <- ""
    ## retain only RGB
    col <- col[1L:3L, ]

  }

  if (linear && input_type %in% c("hex", "other")) {
    sRGB_to_linearRGB <- function(x) {
      x <- x/255
      y <- ((x + 0.055)/1.055)^2.4
      small <- x <= 0.03928
      y[small] <- x[small]/12.92
      return(y * 255)
    }
    col <- sRGB_to_linearRGB(col)
  }

  ## transform color
  RGB <- cvd_transform %*% col
  rownames(RGB) <- c("R", "G", "B")

  ## bound RGB values
  RGB[RGB < 0]   <- 0
  RGB[RGB > 255] <- 255

  if (linear && input_type %in% c("hex", "other")) {
    linearRGB_to_sRGB <- function(y) {
      y <- y/255
      x <- 1.055 * y^(1/2.4) - 0.055
      small <- y <= 0.03928/12.92
      x[small] <- 12.92 * y[small]
      return(x * 255)
    }
    RGB <- linearRGB_to_sRGB(RGB)
  }

  ## convert back to input type
  if (input_type == "colorspace") {
    col <- t(RGB/255)
    col <- if(linear) RGB(col) else sRGB(col)
    col <- as(col, color_class)
  } else if (input_type == "matrix") {
    col <- if(transpose) t(RGB) else RGB
  } else {
    RGB <- round(RGB)
    col <- paste(grDevices::rgb(RGB[1L, ], RGB[2L, ], RGB[3L, ], maxColorValue = 255), alpha, sep = "")
    if(length(NAidx) > 0L) col[NAidx] <- NA
  }

  return(col)
}

#' @rdname simulate_cvd
#' @export
deutan <- function(col, severity = 1, linear = TRUE) {
  simulate_cvd(col, cvd_transform = interpolate_cvd_transform(deutanomaly_cvd, severity), linear = linear)
}

#' @rdname simulate_cvd
#' @export
protan <- function(col, severity = 1, linear = TRUE) {
  simulate_cvd(col, cvd_transform = interpolate_cvd_transform(protanomaly_cvd, severity), linear = linear)
}

#' @rdname simulate_cvd
#' @export
tritan <- function(col, severity = 1, linear = TRUE) {
  simulate_cvd(col, cvd_transform = interpolate_cvd_transform(tritanomaly_cvd, severity), linear = linear)
}

#' @rdname simulate_cvd
#' @export
interpolate_cvd_transform <- function(cvd, severity = 1) {
  if (severity <= 0) {
    cvd[[1]]
  } else if (severity >= 1) {
    cvd[[11]]
  } else {
    s <- 10*severity
    i1 <- floor(s)
    i2 <- ceiling(s)
    if (i1 == i2) {
      cvd[[i1+1]]
    }
    else {
      (i2-s)*cvd[[i1+1]] + (s-i1)*cvd[[i2+1]]
    }
  }
}
