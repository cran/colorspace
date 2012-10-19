\name{LAB}
\alias{LAB}
\title{Create LAB Colors}
\description{
  This function creates colors of class ``LAB'';
  a subclass of the virtual ``color'' class.
}
\usage{
LAB(L, A, B, names)
}
\arguments{
  \item{L,A,B}{these arguments give the L, A and B coordinates of
    the colors.  The values can be provided in separate \code{L},
    \code{A} and \code{B} vectors or in a three-column matrix
    passed as \code{L}.}
  \item{names}{a vector of names for the colors (by default the
    row names of \code{L} are used).}
}
\details{
  The \code{L}, \code{A} and \code{B} values give the coordinates of
  the colors in the CIE \eqn{L^*a^*b^*}{L*a*b*} space.
  This is a transformation of the 1931 CIE XYZ space which attempts to
  produce perceptually based axes.
  Luminance takes values between 0 and 100, and the other coordinates
  take values between -100 and 100.  The \eqn{a} and \eqn{b} coordinates
  measure positions on green/red and blue/yellow axes.
}
\value{
  An object of class ``LAB'' which inherits from class ``color.''
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
  \code{\link{LUV}},
  \code{\link{polarLUV}}.
}

\examples{
## Show the LAB space
set.seed(1)
x <- RGB(runif(1000), runif(1000), runif(1000))
y <- as(x, "LAB")
head(x)
head(y)
plot(y)
}
\keyword{color}
