\name{LUV}
\alias{LUV}
\title{Create LUV Colors}
\description{
  This function creates colors of class ``LUV'';
  a subclass of the virtual ``color'' class.
}
\usage{
LUV(L, U, V, names)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{L,U,V}{these arguments give the L, U and V coordinates of
    the colors.  The values can be provided in separate \code{L},
    \code{U} and \code{V} vectors or in a three-column matrix
    passed as \code{L}.}
  \item{names}{a vector of names for the colors (by default the
    row names of \code{L} are used).}
}
\details{
  The \code{L}, \code{U} and \code{V} values give the coordinates of
  the colors in the CIE (1976) \eqn{L^*u^*v^*}{L*u*v*} space.
  This is a transformation of the 1931 CIE XYZ space which attempts to
  produce perceptually based axes.
  Luminance takes values between 0 and 100, and the other coordinates
  take values between -100 and 100.  The \eqn{a} and \eqn{b} coordinates
  measure positions on green/red and blue/yellow axes.
}
\value{
  An object of class ``LUV'' which inherits from class ``color.''
}
%\references{ ~put references to the literature/web site here ~ }
\author{Ross Ihaka}
%\note{ ~~further notes~~ }

\seealso{
  \code{\link{RGB}},
  \code{\link{HSV}},
  \code{\link{XYZ}},
  \code{\link{LAB}},
  \code{\link{polarLAB}},
  \code{\link{polarLUV}}.
}

\examples{
## Show the LUV space
set.seed(1)
x <- RGB(runif(1000), runif(1000), runif(1000))
y <- as(x, "LUV")
head(x)
head(y)
plot(y)
}
\keyword{color}
