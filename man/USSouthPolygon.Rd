\name{USSouthPolygon}
\alias{USSouthPolygon}

\title{Polygon for County Map of US South States: Alabama, Georgia, and South Carolina}

\description{
County polygons for Alabama, Georgia, and South Carolina plus an artifical
variable used for coloring.
}

\usage{data("USSouthPolygon")}

\format{
A data frame with coordinates of the vertices of the county polygons
(\code{x}, \code{y}) and an artificial variable \code{z} constructed
for illustrating colored maps.
}

\source{
Polygon data taken from \pkg{maps} package of Becker, Wilks, Brownrigg,
and Minka (2012). Version 2.2-6. \url{http://CRAN.R-project.org/package=maps}
}

\examples{
## generate color palette
pal <- diverge_hcl(9)
n <- length(pal)

## draw shaded polygons
plot(0, 0, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n",
  xlim = c(-88.5, -78.6), ylim = c(30.2, 35.2), asp = 1)
polygon(USSouthPolygon, col = pal[cut(na.omit(USSouthPolygon$z), breaks = 0:n/n)])
}

\keyword{datasets}
