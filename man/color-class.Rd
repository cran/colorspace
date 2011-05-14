\name{color-class}
\docType{class}
\alias{color-class}
\alias{RGB-class}
\alias{sRGB-class}
\alias{XYZ-class}
\alias{HSV-class}
\alias{HLS-class}
\alias{LAB-class}
\alias{LUV-class}
\alias{polarLAB-class}
\alias{polarLUV-class}
\alias{[,color-method}
\alias{coerce,color,RGB-method}
\alias{coerce,color,sRGB-method}
\alias{coerce,color,XYZ-method}
\alias{coerce,color,LAB-method}
\alias{coerce,color,polarLAB-method}
\alias{coerce,color,HSV-method}
\alias{coerce,color,HLS-method}
\alias{coerce,color,LUV-method}
\alias{coerce,color,polarLUV-method}
\alias{coords,color-method}
\alias{plot,color-method}
\alias{show,color-method}

\title{Class "color"}
\description{
Objects from the class \emph{color} represent colors in a number of
color spaces.  In particular, there are subclasses of color
which correspond to RGB, HSV, HLS, CIEXYZ, CIELUV, CIELAB and polar
versions of the last two spaces.
}
\section{Objects from the Class}{
  Objects can be created by calls to the functions \code{RGB},
  \code{sRGB}, \code{HSV}, \code{HLS},
  \code{XYZ}, \code{LUV}, \code{LAB}, \code{polarLUV}, and
  \code{polarLAB}.  These are all subclasses of the virtual
  class \emph{color}.
}
\section{Slots}{
  \describe{
    \item{\code{coords}:}{An object of class \code{"matrix"}.}
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "color")}: This method makes it possible to
      take subsets of a vector of colors.}
    \item{coerce}{\code{signature(from = "color", to = "RGB")}: convert
      a color vector to RGB.}
    \item{coerce}{\code{signature(from = "color", to = "sRGB")}: convert
      a color vector to sRGB.}
    \item{coerce}{\code{signature(from = "color", to = "XYZ")}: convert
      a color vector to XYZ.}
    \item{coerce}{\code{signature(from = "color", to = "LAB")}: convert
      a color vector to LAB. }
    \item{coerce}{\code{signature(from = "color", to = "polarLAB")}: convert
      a color vector to polarLAB. }
    \item{coerce}{\code{signature(from = "color", to = "HSV")}: convert
      a color vector to HSV. }
    \item{coerce}{\code{signature(from = "color", to = "HLS")}: convert
      a color vector to HLS. }
    \item{coerce}{\code{signature(from = "color", to = "LUV")}: convert
      a color vector to LUV. }
    \item{coerce}{\code{signature(from = "color", to = "polarLUV")}: convert
      a color vector to polarLUV. }
    \item{coords}{\code{signature(color = "color")}: extract the color
      coordinates from a color vector.}
    \item{plot}{\code{signature(x = "color")}: plot a color vector }
    \item{show}{\code{signature(object = "color")}: show a color vector. }
  }
}
\author{Ross Ihaka}

\seealso{
  \code{\link{RGB}},
  \code{\link{XYZ}},
  \code{\link{HSV}},
  \code{\link{HLS}},
  \code{\link{LAB}},
  \code{\link{polarLAB}},
  \code{\link{LUV}},
  \code{\link{polarLUV}},
  \code{\link{mixcolor}}.
}
\examples{
x = RGB(runif(1000),runif(1000),runif(1000))
plot(as(x, "LUV"))
}
\keyword{classes}
