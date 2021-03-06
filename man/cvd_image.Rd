% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cvd_image.R
\name{cvd_image}
\alias{cvd_image}
\title{Convert Colors of an Image}
\usage{
cvd_image(img, type, file, severity = 1)
}
\arguments{
\item{img}{\code{array} as returned by \code{readPNG} and \code{readJPEG}
of size \code{height x width x depth}. The depth coordinate contains
R/G/B and alpha if given (png).}

\item{type}{\code{string} name of the function which will be used to
convert the colors (\code{deutan}, \code{protan}, \code{tritan}, \code{desaturate}).
If set to \code{original} the image will be written as is.}

\item{file}{\code{string} with (full) path to resulting image. Has to
be a png image name.}

\item{severity}{numeric. Severity of the color vision defect, a number between 0 and 1.}
}
\description{
Used in \code{cvd_emulator}. Takes an image object and converts
the colors using \code{\link{deutan}}, \code{\link{protan}},
\code{\link{tritan}}, \code{\link{desaturate}} functions. The image
will be written to disc as a PNG file.
}
