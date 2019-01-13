##  Copyright 2005, Ross Ihaka. All Rights Reserved.
##  
##  Redistribution and use in source and binary forms, with or without
##  modification, are permitted provided that the following conditions
##  are met:
##  
##     1. Redistributions of source code must retain the above copyright notice,
##        this list of conditions and the following disclaimer.
##  
##     2. Redistributions in binary form must reproduce the above copyright
##        notice, this list of conditions and the following disclaimer in the
##        documentation and/or other materials provided with the distribution.
##  
##     3. The name of the Ross Ihaka may not be used to endorse or promote
##        products derived from this software without specific prior written
##        permission.
##  
##  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS''
##  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
##  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
##  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL ROSS IHAKA BE LIABLE FOR
##  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
##  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
##  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
##  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
##  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
##  IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
##  POSSIBILITY OF SUCH DAMAGE.

##  ----------------------------------------------------------------------------

##  An S4 Color Space Class
##
##  The following color spaces are available:
##
##     RGB       linearized sRGB space
##     sRGB      Gamma-corrected sRGB space
##     XYZ       CIE-XYZ space
##     LAB       CIE-L*a*b* space
##     polarLAB  CIE-L*a*b* space in polar coordinates
##     HSV       Hue-Saturation-Value
##     LUV       CIE-L*u*v*
##     polarLUV  CIE-L*u*v* in polar coordinates (HCL)
##     HLS       Hue-Lightness-Saturation
##
##  The ``canonical'' space here is really CIE-XYZ, but in this
##  implementation all spaces are treated equally, because
##  they are all useful.
##

##  ----------------------------------------------------------------------------

#' Class "color"
#' 
#' Objects from the class \emph{color} represent colors in a number of color
#' spaces.  In particular, there are subclasses of color which correspond to
#' RGB, HSV, HLS, CIEXYZ, CIELUV, CIELAB and polar versions of the last two
#' spaces.
#' 
#' 
#' @name color-class
#' @aliases color-class RGB-class sRGB-class XYZ-class HSV-class HLS-class
#' LAB-class LUV-class polarLAB-class polarLUV-class [,color-method
#' coerce,color,RGB-method coerce,color,sRGB-method coerce,color,XYZ-method
#' coerce,color,LAB-method coerce,color,polarLAB-method coerce,color,HSV-method
#' coerce,color,HLS-method coerce,color,LUV-method coerce,color,polarLUV-method
#' coords,color-method plot,color-method show,color-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls to the
#' functions \code{RGB}, \code{sRGB}, \code{HSV}, \code{HLS}, \code{XYZ},
#' \code{LUV}, \code{LAB}, \code{polarLUV}, and \code{polarLAB}.  These are all
#' subclasses of the virtual class \emph{color}.
#' @section Slots: \describe{
#'     \item{\code{coords}:}{An object of class \code{"matrix"}.}
#'   }
#' @section Methods: \describe{
#'    \item{[}{\code{signature(x = "color")}: This method makes it possible to
#'      take subsets of a vector of colors.}
#'    \item{coerce}{\code{signature(from = "color", to = "RGB")}: convert
#'      a color vector to RGB.}
#'    \item{coerce}{\code{signature(from = "color", to = "sRGB")}: convert
#'      a color vector to sRGB.}
#'    \item{coerce}{\code{signature(from = "color", to = "XYZ")}: convert
#'      a color vector to XYZ.}
#'    \item{coerce}{\code{signature(from = "color", to = "LAB")}: convert
#'      a color vector to LAB. }
#'    \item{coerce}{\code{signature(from = "color", to = "polarLAB")}: convert
#'      a color vector to polarLAB. }
#'    \item{coerce}{\code{signature(from = "color", to = "HSV")}: convert
#'      a color vector to HSV. }
#'    \item{coerce}{\code{signature(from = "color", to = "HLS")}: convert
#'      a color vector to HLS. }
#'    \item{coerce}{\code{signature(from = "color", to = "LUV")}: convert
#'      a color vector to LUV. }
#'    \item{coerce}{\code{signature(from = "color", to = "polarLUV")}: convert
#'      a color vector to polarLUV. }
#'    \item{coords}{\code{signature(color = "color")}: extract the color
#'      coordinates from a color vector.}
#'    \item{plot}{\code{signature(x = "color")}: plot a color vector }
#'    \item{show}{\code{signature(object = "color")}: show a color vector. }
#'  }
#' @author Ross Ihaka
#' @seealso \code{\link{RGB}}, \code{\link{XYZ}}, \code{\link{HSV}},
#' \code{\link{HLS}}, \code{\link{LAB}}, \code{\link{polarLAB}},
#' \code{\link{LUV}}, \code{\link{polarLUV}}, \code{\link{mixcolor}}.
#' @keywords classes
#' @examples
#' x <- RGB(runif(1000), runif(1000), runif(1000))
#' plot(as(x, "LUV"))
#' @useDynLib colorspace, .registration = TRUE 
#' @import methods

#' @export
setClass("color", representation = list(coords = "matrix"))

#' @export
setClass("RGB", contains = "color")

#' @export
setClass("sRGB", contains = "color")

#' @export
setClass("XYZ", contains = "color")

#' @export
setClass("LAB", contains = "color")

#' @export
setClass("polarLAB", contains = "color")

#' @export
setClass("HSV", contains = "color")

#' @export
setClass("HLS", contains = "color")

#' @export
setClass("LUV", contains = "color")

#' @export
setClass("polarLUV", contains = "color")


##  ----------------------------------------------------------------------------

#' Extract the Numerical Coordinates of a Color
#' 
#' This function returns a matrix with three columns which give the coordinates
#' of a color in its natural color space.
#' 
#' 
#' @param color A color.
#' @return A numeric matrix giving the coordinates of the color.
#' @author Ross Ihaka
#' @seealso \code{\link{RGB}}, \code{\link{XYZ}}, \code{\link{LAB}},
#' \code{\link{polarLAB}}, \code{\link{LUV}}, \code{\link{polarLUV}},
#' \code{\link{mixcolor}}.
#' @keywords color
#' @examples
#' x <- RGB(1, 0, 0)
#' coords(as(x, "HSV"))
#' @export coords
#' @importFrom graphics pairs
setGeneric("coords", function(color) standardGeneric("coords"))

#' @export
setMethod("coords", "color", function(color) {color@coords})

#' @export
setMethod("show", "color", function(object) show(coords(object)))

#' @export
setMethod("[", "color",
          function(x, i, j, drop=FALSE) {
            do.call(class(x), list(coords(x)[i,,drop=FALSE]))
          })

#' @export
setMethod("plot", signature("color"),
          function(x, y, pch=20, cex=3)
          pairs(coords(x), col=hex(x,fixup=TRUE), pch=pch, cex=cex))


##  ----------------------------------------------------------------------------

## Auxiliary functions
CheckMatrix <-
  function(x)
  if (!is.double(x) || length(x) < 1
      || length(dim(x)) != 2 || dim(x)[2] != 3)
  stop("invalid color matrix")

CheckBounds <-
  function(x, lower, upper)
  if (any(x < lower | x > upper, na.rm=TRUE))
  warning("out of gammut color")


##  ----------------------------------------------------------------------------

#' Create RGB Colors
#' 
#' This function creates colors of class RGB; a subclass of the virtual
#' \code{\link{color-class}} class.
#' 
#' This function creates colors in the linearized sRGB color space (IEC
#' standard 61966).
#' 
#' @param R,G,B these arguments give the red, green and blue intensities of the
#' colors (the values should lie between 0 and 1).  The values can be provided
#' in separate \code{R}, \code{G} and \code{B} vectors or in a three-column
#' matrix passed as \code{R}.
#' @param names A vector of names for the colors (by default the row names of
#' \code{R} are used).
#' @return An object of class \code{RGB} which inherits from class \code{color}.
#' @author Ross Ihaka
#' @seealso \code{\link{sRGB}}, \code{\link{HSV}}, \code{\link{XYZ}},
#' \code{\link{LAB}}, \code{\link{polarLAB}}, \code{\link{LUV}},
#' \code{\link{polarLUV}}.
#' @keywords color
#' @examples
#' # Create a random set of colors
#' set.seed(1)
#' RGB(R = runif(20), G = runif(20), B = runif(20))
#' @export RGB
RGB <-
  function(R, G, B, names)
  {
    if (missing(R)) return(new("RGB"))
    if (missing(names)) names = dimnames(R)[[1]]
    if (is.integer(R)) R[] <- as.numeric(R)
    coords = cbind(R, if (missing(G)) NULL else G,
                      if (missing(B)) NULL else B)
    CheckMatrix(coords)
    dimnames(coords) = list(names, c("R", "G", "B"))
    new("RGB", coords = coords)
  }



#' Create sRGB Colors
#' 
#' This function creates colors of class sRGB; a subclass of the virtual
#' \code{\link{color-class}} class.
#' 
#' This function creates colors in the standard sRGB color space (IEC standard
#' 61966).
#' 
#' @param R,G,B these arguments give the red, green and blue intensities of the
#' colors (the values should lie between 0 and 1).  The values can be provided
#' in separate \code{R}, \code{G} and \code{B} vectors or in a three-column
#' matrix passed as \code{R}.
#' @param names A vector of names for the colors (by default the row names of
#' \code{R} are used).
#' @return An object of class \code{sRGB} which inherits from class \code{color}.
#' @author Ross Ihaka
#' @seealso \code{\link{RGB}}, \code{\link{HSV}}, \code{\link{XYZ}},
#' \code{\link{LAB}}, \code{\link{polarLAB}}, \code{\link{LUV}},
#' \code{\link{polarLUV}}.
#' @keywords color
#' @examples
#' # Create a random set of colors
#' set.seed(1)
#' sRGB(R = runif(20), G = runif(20), B = runif(20))
#' @export sRGB
sRGB <-
  function(R, G, B, names)
  {
    if (missing(R)) return(new("sRGB"))
    if (missing(names)) names = dimnames(R)[[1]]
    if (is.integer(R)) R[] <- as.numeric(R)
    coords = cbind(R, if (missing(G)) NULL else G,
                      if (missing(B)) NULL else B)
    CheckMatrix(coords)
    dimnames(coords) = list(names, c("R", "G", "B"))
    new("sRGB", coords = coords)
  }



#' Create XYZ Colors
#' 
#' This function creates colors of class XYZ; a subclass of the virtual
#' \code{\link{color-class}} class.
#' 
#' The X, Y and Z values are the levels of the CIE primaries.  These are scaled
#' so that the luminance of the display white-point is 100.  The white-point is
#' taken to be D65, which means that its coordinates are 95.047, 100.000,
#' 108.883.
#' 
#' @param X,Y,Z these arguments give the X, Y and Z coordinates of the colors.
#' The values can be provided in separate \code{X}, \code{Y} and \code{Z}
#' vectors or in a three-column matrix passed as \code{X}.
#' @param names A vector of names for the colors (by default the row names of
#' \code{X} are used).
#' @return An object of class \code{XYZ} which inherits from class \code{color}.
#' @author Ross Ihaka
#' @seealso \code{\link{RGB}}, \code{\link{HSV}}, \code{\link{LAB}},
#' \code{\link{polarLAB}}, \code{\link{LUV}}, \code{\link{polarLUV}}.
#' @keywords color
#' @examples
#' ## Generate white in XYZ space
#' XYZ(95.047, 100.000, 108.883)
#' @export XYZ
XYZ <-
  function(X, Y, Z, names)
  {
    if (missing(X)) return(new("XYZ"))
    if (missing(names)) names = dimnames(X)[[1]]
    if (is.integer(X)) X[] <- as.numeric(X)
    coords = cbind(X, if (missing(Y)) NULL else Y,
                      if (missing(Z)) NULL else Z)
    CheckMatrix(coords)
    ## CheckBounds(coords, 0, Inf)
    dimnames(coords) = list(names, c("X", "Y", "Z"))
    new("XYZ", coords = coords)
  }



#' Create LAB Colors
#' 
#' This function creates colors of class ``LAB''; a subclass of the virtual
#' \code{\link{color-class}} class.
#' 
#' The \code{L}, \code{A} and \code{B} values give the coordinates of the
#' colors in the CIE \eqn{L^*a^*b^*}{L*a*b*} space.  This is a transformation
#' of the 1931 CIE XYZ space which attempts to produce perceptually based axes.
#' Luminance takes values between 0 and 100, and the other coordinates take
#' values between -100 and 100.  The \eqn{a} and \eqn{b} coordinates measure
#' positions on green/red and blue/yellow axes.
#' 
#' @param L,A,B these arguments give the L, A and B coordinates of the colors.
#' The values can be provided in separate \code{L}, \code{A} and \code{B}
#' vectors or in a three-column matrix passed as \code{L}.
#' @param names a vector of names for the colors (by default the row names of
#' \code{L} are used).
#' @return An object of class \code{LAB} which inherits from class \code{color}.
#' @author Ross Ihaka
#' @seealso \code{\link{RGB}}, \code{\link{HSV}}, \code{\link{XYZ}},
#' \code{\link{LAB}}, \code{\link{polarLAB}}, \code{\link{LUV}},
#' \code{\link{polarLUV}}.
#' @keywords color
#' @examples
#' ## Show the LAB space
#' set.seed(1)
#' x <- RGB(runif(1000), runif(1000), runif(1000))
#' y <- as(x, "LAB")
#' head(x)
#' head(y)
#' plot(y)
#' @export LAB
LAB <-
  function(L, A, B, names)
  {
    if (missing(L)) return(new("LAB"))
    if (missing(names)) names = dimnames(L)[[1]]
    if (is.integer(L)) L[] <- as.numeric(L)
    coords = cbind(L, if (missing(A)) NULL else A,
                      if (missing(B)) NULL else B)
    CheckMatrix(coords)
    ## CheckBounds(coords[,1], 0, 100)
    ## CheckBounds(coords[,2], -500, 500)
    ## CheckBounds(coords[,3], -200, 200)
    dimnames(coords) = list(names, c("L", "A", "B"))
    new("LAB", coords = coords)
  }



#' Create polarLAB Colors
#' 
#' This function creates colors of class ``polarLAB''; a subclass of the
#' virtual \code{\link{color-class}} class.
#' 
#' The polarLAB space is a transformation of the CIE \eqn{L^*a^*b^*}{L*a*b*}
#' space so that the \eqn{a} and \eqn{b} values are converted to polar
#' coordinates.  The radial component \eqn{C} measures chroma and the angular
#' coordinate \eqn{H} is measures hue.
#' 
#' @param L,C,H these arguments give the L, C and H coordinates of the colors.
#' The values can be provided in separate \code{L}, \code{C} and \code{H}
#' vectors or in a three-column matrix passed as \code{L}.
#' @param names A vector of names for the colors (by default the row names of
#' \code{L} are used).
#' @return An object of class \code{polarLAB} which inherits from class \code{color}.
#' @author Ross Ihaka
#' @seealso \code{\link{RGB}}, \code{\link{HSV}}, \code{\link{XYZ}},
#' \code{\link{LAB}}, \code{\link{polarLAB}}, \code{\link{LUV}},
#' \code{\link{polarLUV}}.
#' @keywords color
#' @examples
#' ## Show the polarLAB space
#' set.seed(1)
#' x <- RGB(runif(1000), runif(1000), runif(1000))
#' y <- as(x, "polarLAB")
#' head(x)
#' head(y)
#' plot(y)
#' @export polarLAB
polarLAB <-
  function(L, C, H, names)
  {
    if (missing(L)) return(new("polarLAB"))
    if (missing(names)) names = dimnames(L)[[1]]
    if (is.integer(L)) L[] <- as.numeric(L)
    coords = cbind(L, if (missing(C)) NULL else C,
                      if (missing(H)) NULL else H)
    CheckMatrix(coords)
    ## CheckBounds(coords[,1], 0, Inf)
    ## CheckBounds(coords[,2], 0, Inf)
    ## CheckBounds(coords[,3], 0, 360)
    dimnames(coords) = list(names, c("L", "C", "H"))
    new("polarLAB", coords = coords)
  }



#' Create HSV Colors
#' 
#' This function creates colors of class HSV; a subclass of the virtual
#' \code{\link{color-class}} class.
#' 
#' This function creates colors in the HSV color space which corresponds to the
#' standard sRGB color space (IEC standard 61966).  The hues should lie between
#' between 0 and 360, and the saturations and values should lie between 0 and
#' 1.
#' 
#' @param H,S,V These arguments give the hue, saturation and value of the
#' colors. The values can be provided in separate \code{H}, \code{S} and
#' \code{V} vectors or in a three-column matrix passed as \code{H}.
#' @param names A vector of names for the colors (by default the row names of
#' \code{H} are used).
#' @return An object of class \code{HSV} which inherits from class \code{color}.
#' @author Ross Ihaka
#' @seealso \code{\link{RGB}}, \code{\link{XYZ}}, \code{\link{LAB}},
#' \code{\link{polarLAB}}, \code{\link{LUV}}, \code{\link{polarLUV}}.
#' @keywords color
#' @examples
#' # A rainbow of full-intensity hues
#' HSV(seq(0, 360, length = 13)[-13], 1, 1)
#' @export HSV
HSV <-
  function(H, S, V, names)
  {
    if (missing(H)) return(new("HSV"))
    if (missing(names)) names = dimnames(H)[[1]]
    if (is.integer(H)) H[] <- as.numeric(H)
    coords = cbind(H, if (missing(S)) NULL else S,
                      if (missing(V)) NULL else V)
    CheckMatrix(coords)
    ## CheckBounds(coords[,1], 0, 360)
    ## CheckBounds(coords[,2], 0, 1)
    ## CheckBounds(coords[,3], 0, 1)
    dimnames(coords) = list(names, c("H", "S", "V"))
    new("HSV", coords = coords)
  }



#' Create HLS Colors
#' 
#' This function creates colors of class HLS; a subclass of the virtual
#' \code{\link{color-class}} class.
#' 
#' This function creates colors in the HLS color space which corresponds to the
#' standard sRGB color space (IEC standard 61966).  The hues should lie between
#' between 0 and 360, and the lightness and saturations should lie between 0
#' and 1.
#' 
#' @param H,L,S These arguments give the hue, lightness, and saturation of the
#' colors. The values can be provided in separate \code{H}, \code{L} and
#' \code{S} vectors or in a three-column matrix passed as \code{H}.
#' @param names A vector of names for the colors (by default the row names of
#' \code{H} are used).
#' @return An object of class \code{HLS} which inherits from class \code{color}.
#' @author Ross Ihaka
#' @seealso \code{\link{RGB}}, \code{\link{XYZ}}, \code{\link{LAB}},
#' \code{\link{polarLAB}}, \code{\link{LUV}}, \code{\link{polarLUV}}.
#' @keywords color
#' @examples
#' # A rainbow of full-intensity hues
#' HLS(seq(0, 360, length = 13)[-13], 0.5, 1)
#' @export HLS
HLS <-
  function(H, L, S, names)
  {
    if (missing(H)) return(new("HLS"))
    if (missing(names)) names = dimnames(H)[[1]]
    if (is.integer(H)) H[] <- as.numeric(H)
    coords = cbind(H, if (missing(L)) NULL else L,
                      if (missing(S)) NULL else S)
    CheckMatrix(coords)
    ## CheckBounds(coords[,1], 0, 360)
    ## CheckBounds(coords[,2], 0, 1)
    ## CheckBounds(coords[,3], 0, 1)
    dimnames(coords) = list(names, c("H", "L", "S"))
    new("HLS", coords = coords)
  }



#' Create LUV Colors
#' 
#' This function creates colors of class ``LUV''; a subclass of the virtual
#' \code{\link{color-class}} class.
#' 
#' The \code{L}, \code{U} and \code{V} values give the coordinates of the
#' colors in the CIE (1976) \eqn{L^*u^*v^*}{L*u*v*} space.  This is a
#' transformation of the 1931 CIE XYZ space which attempts to produce
#' perceptually based axes.  Luminance takes values between 0 and 100, and the
#' other coordinates take values between -100 and 100.  The \eqn{a} and \eqn{b}
#' coordinates measure positions on green/red and blue/yellow axes.
#' 
#' @param L,U,V these arguments give the L, U and V coordinates of the colors.
#' The values can be provided in separate \code{L}, \code{U} and \code{V}
#' vectors or in a three-column matrix passed as \code{L}.
#' @param names a vector of names for the colors (by default the row names of
#' \code{L} are used).
#' @return An object of class \code{LUV} which inherits from class \code{color}.
#' @author Ross Ihaka
#' @seealso \code{\link{RGB}}, \code{\link{HSV}}, \code{\link{XYZ}},
#' \code{\link{LAB}}, \code{\link{polarLAB}}, \code{\link{polarLUV}}.
#' @keywords color
#' @examples
#' ## Show the LUV space
#' set.seed(1)
#' x <- RGB(runif(1000), runif(1000), runif(1000))
#' y <- as(x, "LUV")
#' head(x)
#' head(y)
#' plot(y)
#' @export LUV
LUV <-
  function(L, U, V, names)
  {
    if (missing(L)) return(new("LUV"))
    if (missing(names)) names = dimnames(L)[[1]]
    if (is.integer(L)) L[] <- as.numeric(L)
    coords = cbind(L, if (missing(U)) NULL else U,
                      if (missing(V)) NULL else V)
    ## CheckBounds(coords[,1], 0, 100)
    ## CheckBounds(coords[,2], -Inf, Inf)
    ## CheckBounds(coords[,3], -Inf, Inf)
    dimnames(coords) = list(names, c("L", "U", "V"))
    new("LUV", coords = coords)
  }



#' Create polarLUV (HCL) Colors
#' 
#' This function creates colors of class ``polarLUV''; a subclass of the
#' virtual \code{\link{color-class}} class.
#' 
#' The polarLUV space is a transformation of the CIE \eqn{L^*u^*u^*}{L*u*u*}
#' space so that the \eqn{u} and \eqn{u} values are converted to polar
#' coordinates.  The radial component \eqn{C} measures chroma and the angular
#' coordinate \eqn{H} is measures hue. It is also known as the HCL
#' (hue-chroma-luminance) space.
#' 
#' @param L,C,H these arguments give the L, C and H coordinates of the colors.
#' The values can be provided in separate \code{L}, \code{C} and \code{H}
#' vectors or in a three-column matrix passed as \code{L}.
#' @param names A vector of names for the colors (by default the row names of
#' \code{L} are used).
#' @return An object of class \code{polarLUV} which inherits from class \code{color}.
#' @author Ross Ihaka
#' @seealso \code{\link{RGB}}, \code{\link{HSV}}, \code{\link{XYZ}},
#' \code{\link{LAB}}, \code{\link{polarLAB}}, \code{\link{LUV}},
#' \code{\link{polarLUV}}.
#' @keywords color
#' @examples
#' ## Show the polarLUV space
#' set.seed(1)
#' x <- RGB(runif(1000), runif(1000), runif(1000))
#' y <- as(x, "polarLUV")
#' head(x)
#' head(y)
#' plot(y)
#' @export polarLUV
polarLUV <-
  function(L, C, H, names)
  {
    if (missing(L)) return(new("polarLUV"))
    if (missing(names)) names = dimnames(L)[[1]]
    if (is.integer(L)) L[] <- as.numeric(L)
    coords = cbind(L, if (missing(C)) NULL else C,
                      if (missing(H)) NULL else H)
    dimnames(coords) = list(names, c("L", "C", "H"))
    new("polarLUV", coords = coords)
  }


##  ----------------------------------------------------------------------------

setAs("color", "RGB", function(from)
      RGB(.Call("as_RGB", from@coords, class(from), .whitepoint$white,
                PACKAGE = "colorspace"),
          names = dimnames(from@coords)[[1]]))

setAs("color", "sRGB", function(from)
      sRGB(.Call("as_sRGB", from@coords, class(from), .whitepoint$white,
                 PACKAGE = "colorspace"),
           names = dimnames(from@coords)[[1]]))

setAs("color", "XYZ", function(from)
      XYZ(.Call("as_XYZ", from@coords, class(from), .whitepoint$white,
                PACKAGE = "colorspace"),
          names = dimnames(from@coords)[[1]]))

setAs("color", "LAB", function(from)
      LAB(.Call("as_LAB", from@coords, class(from), .whitepoint$white,
                PACKAGE = "colorspace"),
          names = dimnames(from@coords)[[1]]))

setAs("color", "polarLAB", function(from)
      polarLAB(.Call("as_polarLAB", from@coords, class(from),
                     .whitepoint$white, PACKAGE = "colorspace"),
               names = dimnames(from@coords)[[1]]))

setAs("color", "HSV", function(from)
      HSV(.Call("as_HSV", from@coords, class(from), .whitepoint$white,
                PACKAGE = "colorspace"),
          names = dimnames(from@coords)[[1]]))

setAs("color", "HLS", function(from)
      HLS(.Call("as_HLS", from@coords, class(from), .whitepoint$white,
                PACKAGE = "colorspace"),
          names = dimnames(from@coords)[[1]]))

setAs("color", "LUV", function(from)
      LUV(.Call("as_LUV", from@coords, class(from), .whitepoint$white,
                PACKAGE = "colorspace"),
          names = dimnames(from@coords)[[1]]))

setAs("color", "polarLUV", function(from)
      polarLUV(.Call("as_polarLUV", from@coords, class(from),
                     .whitepoint$white, PACKAGE = "colorspace"),
               names = dimnames(from@coords)[[1]]))


##  ----------------------------------------------------------------------------

#' Convert Colors to Hexadecimal Strings
#' 
#' This functions converts \code{\link{color-class}} objects into hexadecimal strings.
#' 
#' The color objects are first converted to sRGB color objects.  They are then
#' multiplied by 255 and rounded to obtain an integer value.  These values are
#' then converted to hexadecimal strings of the form \code{"#RRGGBB"} and
#' suitable for use as color descriptions for R graphics.  Out of gamut values
#' are either corrected to valid RGB values by translating the the individual
#' primary values so that they lie between 0 and 255.
#' 
#' @param from The color object to be converted.
#' @param gamma Deprecated.
#' @param fixup Should the color be corrected to a valid RGB value before
#' correction.  The default is to convert out-of-gamut colors to the string
#' \code{"NA"}.
#' @return A vector of character strings.
#' @author Ross Ihaka
#' @seealso \code{\link{hex2RGB}}, \code{\link{RGB}}, \code{\link{sRGB}},
#' \code{\link{HSV}}, \code{\link{XYZ}}, \code{\link{LAB}},
#' \code{\link{polarLAB}}, \code{\link{LUV}}, \code{\link{polarLUV}}.
#' @keywords color
#' @examples
#' hsv <- HSV(seq(0, 360, length = 7)[-7], 1, 1)
#' hsv
#' hex(hsv)
#' barplot(rep(1,6), col = hex(hsv))
#' @export hex
hex <-
  function(from, gamma = NULL, fixup = FALSE)
  {
      if (!is.null(gamma))
          warning("'gamma' is deprecated and has no effect")
      coords <- as(from, "sRGB")@coords
      rval <- .Call("sRGB_to_RColor", coords, fixup, PACKAGE = "colorspace")
      if(!is.null(dnam <- attr(coords, "dimnames"))) names(rval) <- dnam[[1L]]
      return(rval)
  }



#' Convert Hexadecimal Color Specifications to sRGB Objects
#' 
#' This function takes a vector of strings of the form \code{"#RRGGBB"}
#' (hexadecimal color descriptions) into \code{\link{sRGB}} objects.
#' 
#' This function converts device-dependent color descriptions of the form
#' \code{"#RRGGBB"} into sRGB color descriptions (linearized if \code{gamma} is
#' \code{TRUE}).  The alpha channel will be ignored if given
#' (\code{"#RRGGBBAA"}).
#' 
#' @param x a vector of hexadecimal color descriptions.
#' @param gamma Whether to apply gamma-correction.
#' @return An sRGB object describing the colors.
#' @author Ross Ihaka
#' @seealso \code{\link{hex}}, \code{\link{RGB}}, \code{\link{sRGB}},
#' \code{\link{HSV}}, \code{\link{XYZ}}, \code{\link{polarLAB}},
#' \code{\link{LUV}}, \code{\link{polarLUV}}.
#' @keywords color
#' @examples
#' hex2RGB(c("#FF0000","#00FF00", "#0000FF50"))
#' @export hex2RGB
hex2RGB =
  function(x, gamma = FALSE) {
      x <- substr(x,0,7) # Remove alpha
      rval <- if (gamma)
          RGB(.Call("hex_to_RGB", x, gamma, PACKAGE = "colorspace"))
      else
          sRGB(.Call("hex_to_RGB", x, gamma, PACKAGE = "colorspace"))
      if(!is.null(nam <- names(x))) attr(rval@coords, "dimnames")[[1L]] <- nam
      return(rval)
  }
  



#' Read RGB Color Descriptions
#' 
#' This function reads a set of RGB color descriptions (of the form written by
#' \code{gcolorsel}) from a file and creates a color object containing the
#' corresponding colors.
#' 
#' The file is assumed to contain RGB color descriptions consisting of three
#' integer values in the range from 0 to 255 followed by a color name.
#' 
#' @param file The file containing the color descriptions.
#' @param class The kind of color object to be returned.
#' @return An color object of the specified class containing the color
#' descriptions.
#' @author Ross Ihaka
#' @seealso \code{\link{writehex}}, \code{\link{readhex}},
#' \code{\link{hex2RGB}}, \code{\link{RGB}}, \code{\link{HSV}},
#' \code{\link{XYZ}}, \code{\link{LAB}}, \code{\link{polarLAB}},
#' \code{\link{LUV}}, \code{\link{polarLUV}}.
#' @keywords color
#' @examples
#' \dontrun{
#' rgb <- readRGB("pastel.rgb")
#' hsv <- readRGB("pastel.rgb", "HSV")
#' }
#' @export readRGB
readRGB <-
  function(file, class="RGB")
  {
      x = scan(file, what=list(R = 0, G = 0, B = 0, name = ""))
      as(RGB(R = x$R/255, G = x$G/255, B = x$B/255), Class=class)
  }



#' Read Hexadecimal Color Descriptions
#' 
#' This function reads a set of hexadecimal color descriptions from a file and
#' creates a color object containing the corresponding colors.
#' 
#' The file is assumed to contain hexadecimal color descriptions of the form
#' \code{#RRGGBB}.
#' 
#' @param file The file containing the color descriptions.
#' @param class The kind of color object to be returned.
#' @return An color object of the specified class containing the color
#' descriptions.
#' @author Ross Ihaka
#' @seealso \code{\link{writehex}}, \code{\link{readRGB}},
#' \code{\link{hex2RGB}}, \code{\link{RGB}}, \code{\link{HSV}},
#' \code{\link{XYZ}}, \code{\link{LAB}}, \code{\link{polarLAB}},
#' \code{\link{LUV}}, \code{\link{polarLUV}},
#' @keywords color
#' @examples
#' \dontrun{
#' rgb <- readhex("pastel.txt")
#' hsv <- readhex("pastel.txt", "HSV")
#' }
#' @export readhex
readhex <-
  function(file="", class="RGB")
  as(hex2RGB(scan(file, what = "")), Class=class)



#' Write Hexadecimal Color Descriptions
#' 
#' Given a color object, this function writes a file containing the hexadecimal
#' representation of the colors in the object.
#' 
#' This function converts the given color object to RGB and then writes
#' hexadecimal strings (of the form \code{#RRGGBB}) representing the colors to
#' the specified file.
#' 
#' @param x a color object.
#' @param file the name of the file to be written.
#' @return The name of the file is returned as the value of the function.
#' @author Ross Ihaka
#' @seealso \code{\link{readhex}}, \code{\link{readRGB}},
#' \code{\link{hex2RGB}}, \code{\link{RGB}}, \code{\link{HSV}},
#' \code{\link{XYZ}}, \code{\link{LAB}}, \code{\link{polarLAB}},
#' \code{\link{LUV}}, \code{\link{polarLUV}}.
#' @keywords color
#' @examples
#' set.seed(1)
#' x <- RGB(runif(10), runif(10), runif(10))
#' writehex(x, "random.txt")
#' @export writehex
writehex <-
  function(x, file="")
  {
      cat(hex(x), sep="\n", file=file)
      file
  }

.whitepoint <- new.env()
#' Access or Modify the Whitepoint
#' 
#' This function can be used to control the single global whitepoint
#' that affects all color conversions within the package (that require
#' a whitepoint, i.e., go through XYZ).
#'
#' @aliases whitepoint
#' @param white,... Either missing (to query the whitepoint) or
#' \code{NULL} or a specification of the XYZ coordinates of the whitepoint
#' (to set the whitepoint, see examples). \code{NULL} corresponds to
#' CIE D65 with XYZ coordinates 95.047, 100.000, 108.883.
#' @return \code{whitepoint} returns an XYZ color object for the whitepoint
#' (invisibly in case a new whitepoint was set).
#' @seealso \code{\link{XYZ}} and \link{color-class}.
#' @keywords color
#' @examples
#' # query current whitepoint (D65 by default)
#' whitepoint()
#' 
#' # Illuminant E 
#' whitepoint(XYZ(100, 100, 100))
#' 
#' # equivalently
#' whitepoint(100, 100, 100)
#' whitepoint(c(100, 100, 100))
#' whitepoint(cbind(100, 100, 100))
#'
#' whitepoint()
#' 
#' ## reset
#' whitepoint(NULL)
#' whitepoint()
#' @export whitepoint
#' @rdname whitepoint
whitepoint <- function(white, ...) {
    ## get whitepoint
    if (missing(white)) return(XYZ(.whitepoint$white))
    
    ## set whitepoint to default
    if (is.null(white)) {
        white <- cbind(95.047, 100.000, 108.883)
    ## construct whitepoint XYZ coordinates from numeric input
    } else if (is.numeric(white)) {
        white <- do.call("cbind", list(white, ...))
        if (all(dim(white) != 3L))
            stop("'white' needs to be a 3-dimensional XYZ specification")
        if (ncol(white) != 3L)
            white <- t(white)
    } else {
        ## coerce to XYZ if necessary
	if (!is(white, "XYZ")) white <- as(white, "XYZ")
        white <- coords(white)
    }
    ## force to single row (color)
    white <- white[1L, , drop = FALSE]
    ## set whitepoint and return XYZ color invisibly
    assign("white", white, envir = .whitepoint)
    invisible(XYZ(white))
}
whitepoint(NULL)

#' Compute the Convex Combination of Two Colors
#' 
#' This function can be used to compute the result of color mixing (it assumes
#' additive mixing).
#' 
#' 
#' @param alpha The mixed color is obtained by combining an amount
#' \code{1 - alpha} of \code{color1} with an amount \code{alpha} of
#' \code{color2}.
#' @param color1 The first color.
#' @param color2 The second color.
#' @param where The color space where the mixing is to take place.
#' @return The mixed color.  This is in the color space specified by
#' \code{where}.
#' @author Ross Ihaka
#' @seealso \code{\link{RGB}}, \code{\link{HSV}}, \code{\link{XYZ}},
#' \code{\link{LAB}}, \code{\link{polarLAB}}, \code{\link{LUV}},
#' \code{\link{polarLUV}}.
#' @keywords color
#' @examples
#' mixcolor(0.5, RGB(1, 0, 0), RGB(0, 1, 0))
#' @export mixcolor
mixcolor <- 
  function(alpha, color1, color2, where = class(color1))
  {
    alpha = as.numeric(alpha)
    c1 = coords(as(color1, where))
    c2 = coords(as(color2, where))
    na = length(alpha)
    n1 = nrow(c1)
    n2 = nrow(c2)
    n = max(na, n1, n2)
    if (na < n) alpha = rep(alpha, length = n)
    if (n1 < n) c1 = c1[rep(1:n1, length = n),]
    if (n2 < n) c2 = c2[rep(1:n2, length = n),]
    get(where)((1 - alpha) * c1 + alpha * c2)
  }
