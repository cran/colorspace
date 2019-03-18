#' Color Palette Demonstration Plot
#' 
#' Demonstration of color palettes in various kinds of statistical graphics.
#' 
#' To demonstrate how different kinds of color palettes work in different
#' kinds of statistical displays, \code{demoplot} provides a simple convenience
#' interface to some base graphics with (mostly artificial) data sets.
#' All types of demos can deal with arbitrarily many colors. However, some
#' displays are much more suitable for a low number of colors (e.g., the pie
#' chart) while others work better with more colors (e.g., the heatmap).
#' 
#' @param x character vector containing color hex codes.
#' @param type character indicating the type of demonstration plot.
#' @param \dots currently not used.
#' @return \code{demoplot} returns invisibly what the respective base graphics
#' functions return that are called internally.
#' @seealso \code{\link{specplot}}, \code{\link{hclplot}}
#' @references Zeileis A, Fisher JC, Hornik K, Ihaka R, McWhite CD, Murrell P, Stauffer R, Wilke CO (2019).
#' \dQuote{ccolorspace: A Toolbox for Manipulating and Assessing Colors and Palettes.}
#' arXiv:1903.06490, arXiv.org E-Print Archive. \url{http://arxiv.org/abs/1903.06490}
#' @keywords hplot
#' @examples
#' ## all built-in demos with the same sequential heat color palette
#' par(mfrow = c(3, 3))
#' cl <- sequential_hcl(5, "Heat")
#' for (i in c("map", "heatmap", "scatter", "spine", "bar", "pie", "perspective", "mosaic", "lines")) {
#'   demoplot(cl, type = i)
#' }
#' 
#' ## qualitative palettes: light pastel colors for shading areas (pie)
#' ## and darker colorful palettes for points or lines
#' demoplot(qualitative_hcl(4, "Pastel 1"), type = "pie")
#' demoplot(qualitative_hcl(4, "Set 2"), type = "scatter")
#' demoplot(qualitative_hcl(4, "Dark 3"), type = "lines")
#' 
#' ## sequential palettes: display almost continuous gradients with
#' ## strong luminance contrasts (heatmap, perspective) and colorful
#' ## sequential palette for spine plot with only a few ordered categories
#' demoplot(sequential_hcl(99, "Purple-Blue"), type = "heatmap")
#' demoplot(sequential_hcl(99, "Reds"), type = "perspective")
#' demoplot(sequential_hcl(4, "Viridis"), type = "spine")
#' 
#' ## diverging palettes: display almost continuous gradient with
#' ## strong luminance contrast bringing out the extremes (map),
#' ## more colorful palette with lower luminance contrasts for displays
#' ## with fewer colors (mosaic, bar)
#' demoplot(diverging_hcl(99, "Tropic", power = 2.5), type = "map")
#' demoplot(diverging_hcl(5, "Green-Orange"), type = "mosaic")
#' demoplot(diverging_hcl(5, "Blue-Red 2"), type = "bar")
#' 
#' ## some palettes that work well on black backgrounds
#' par(mfrow = c(2, 3), bg = "black")
#' demoplot(sequential_hcl(9, "Oslo"), "heatmap")
#' demoplot(sequential_hcl(9, "Turku"), "heatmap")
#' demoplot(sequential_hcl(9, "Inferno", rev = TRUE), "heatmap")
#' demoplot(qualitative_hcl(9, "Set 2"), "lines")
#' demoplot(diverging_hcl(9, "Berlin"), "scatter")
#' demoplot(diverging_hcl(9, "Cyan-Magenta", l2 = 20), "lines")
#' 
#' @export demoplot
#' @importFrom graphics barplot image persp pie plot polygon rect segments
demoplot <- function(x,
  type = c("map", "heatmap", "scatter", "spine", "bar", "pie", "perspective", "mosaic", "lines"),
  ...)
{
  type <- match.arg(type,
    c("map", "heatmap", "scatter", "spine", "bar", "pie", "perspective", "mosaic", "lines"))
  do.call(paste("plot", type, sep = "_"), list(x = x, ...))
}


# Plot map example 
plot_map <- function(x, ...) {
   n <- length(x)
   plot(0, 0, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n",
        xlim = c(-88.5, -78.6), ylim = c(30.2, 35.2), asp = 1)
   polygon(colorspace::USSouthPolygon,
           col = x[cut(stats::na.omit(colorspace::USSouthPolygon$z), 
           breaks = 0:n / n)])
}
  
# Plot heatmap example
plot_heatmap <- function(x, ...) {
   image(datasets::volcano, col = rev(x), bty = "n", xaxt = "n", yaxt = "n", useRaster = TRUE)
}
  
# Plot scatter example
.example_env <- new.env()
.example_env$xyhclust <- NULL
plot_scatter <- function(x, ...) {
  
   # Generate artificial data 
   if (is.null(.example_env$xyhclust)) {
      set.seed(1071)
      x0 <- sin(pi * 1:60 / 30) / 5
      y0 <- cos(pi * 1:60 / 30) / 5
      xr <- c(0.1, -0.6, -0.7, -0.9,  0.4,  1.3, 1.0)
      yr <- c(0.3,  1.0,  0.1, -0.9, -0.8, -0.4, 0.6)
      dat <- data.frame(
        x=c(x0 + xr[1], x0 + xr[2], x0 + xr[3], x0 + xr[4], x0 + xr[5], 
            x0 + xr[6], x0 + xr[7]),
        y=c(y0 + yr[1], y0 + yr[2], y0 + yr[3], y0 + yr[4], y0 + yr[5], 
            y0 + yr[6], y0 + yr[7])
      )
      attr(dat, "hclust") <- stats::hclust(stats::dist(dat), method = "ward.D")
      dat$xerror <- stats::rnorm(nrow(dat), sd=stats::runif(nrow(dat), 0.05, 0.45))
      dat$yerror <- stats::rnorm(nrow(dat), sd=stats::runif(nrow(dat), 0.05, 0.45))
      .example_env$xyhclust <- dat
   }
   plot(.example_env$xyhclust$x +
        .example_env$xyhclust$xerror,
	.example_env$xyhclust$y +
	.example_env$xyhclust$yerror,
        col = "black", bg = x[stats::cutree(attr(.example_env$xyhclust, "hclust"), length(x))],
        xlab = "", ylab = "", axes = FALSE, pch = 21, cex = 1.3)
}
  
# Plot spine example
plot_spine <- function(x, ...) {
   n <- length(x)
   
   # Rectangle dimensions
   off <- 0.015
   widths <- c(0.05, 0.1, 0.15, 0.1, 0.2, 0.08, 0.12, 0.16, 0.04)
   k <- length(widths)
   heights <- sapply(
      c(2.5, 1.2, 2.7, 1, 1.3, 0.7, 0.4, 0.2, 1.7),
      function(p) (0:n / n)^(1 / p)
   )
  
   # Rectangle coordinates
   xleft0 <- c(0, cumsum(widths + off)[-k])
   xleft <- rep(xleft0, each=n)
   xright <- xleft + rep(widths, each=n)
   ybottom <- as.vector(heights[-(n + 1), ])
   ytop <- as.vector(heights[-1, ])
  
   # Draw rectangles, borders, and annotation
   plot(0, 0, xlim=c(0, sum(widths) + off * (k - 1)), ylim=c(0, 1),
        xaxs="i", yaxs="i", main="", xlab="", ylab="",
        type="n", axes=FALSE)
   rect(xleft, ybottom, xright, ytop, col = rep(x, k),
        border = if(n < 10) "black" else "transparent")
   if(n >= 10) rect(xleft0, 0, xleft0 + widths, 1, border="black")
}
  
# Plot bar example
plot_bar <- function(x, ...) {
   barplot(cbind(1.1 + abs(sin(0.5 + seq_along(x))) / 3,
           1.9 + abs(cos(1.1 + seq_along(x))) / 3,
           0.7 + abs(sin(1.5 + seq_along(x))) / 3,
           0.3 + abs(cos(0.8 + seq_along(x))) / 3),
           beside = TRUE, col = x, axes = FALSE)
}

# Plot pie example
plot_pie <- function(x, ...) {
   pie(0.01 + abs(sin(0.5 + seq_along(x))), labels = "", col = x, radius = 1)
}
  
# Plot perspective example
plot_perspective <- function(x, ...) {
   # Mixture of bivariate normals
   n <- 31
   x1 <- x2 <- seq(-3, 3, length = n)
   y <- outer(x1, x2, 
            function(x, y) {
                0.5 * stats::dnorm(x, mean = -1, sd = 0.80) * stats::dnorm(y, mean = -1, sd = 0.80) +
                0.5 * stats::dnorm(x, mean =  1, sd = 0.72) * stats::dnorm(y, mean =  1, sd = 0.72)
            }
        )

   # Compute color based on density
   if (length(x) > 1) {
      facet <- cut(y[-1, -1] + y[-1, -n] + y[-n, -1] + y[-n, -n], 
                   length(x))
      cols <- rev(x)[facet]
   } else {
      cols <- x
   }

   # Perspective plot coding z-axis with color
   persp(x1, x2, y, col = cols, phi = 28, theta = 20, r = 5, xlab = "", ylab = "", zlab = "")
}
  
# Plot mosaic example
.example_env$msc.matrix <- NULL
plot_mosaic <- function(x, ...) {
   if (is.null(.example_env$msc.matrix)) {
      set.seed(1071)
      mat <- list()
      for (i in 1:50) {
         mat[[i]] <- matrix(stats::runif(i * 10, min = -1, max = 1), nrow = 10, ncol = i)
      }
      .example_env$msc.matrix <- mat
   }
   image(.example_env$msc.matrix[[length(x)]], bty = "n", col = x, xaxt = "n", yaxt = "n")
}
  
# Plot lines example
plot_lines <- function(x, ...) {
   n <- length(x)
   plot(NULL, xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", 
        bty = "n", xlim = c(0, 6), ylim = c(1.5, n + 1.5))
   s <- 2:(n + 1)
   rev.s <- rev(s)
   rev.x <- rev(x)
   lwd <- 6
   if (n > 5)
      lwd <- lwd -1
   if (n > 15)
      lwd <- lwd -1
   if (n > 25)
      lwd <- lwd -1
   segments(1 / s, s, 2 + 1 / rev.s, rev.s, x, lwd = lwd)
   segments(2 + 1 / s, s, 4 - 1 / s, s, rev.x, lwd = lwd)
   segments(4 - 1 / s, s, 6 - 1 / s, rev.s, rev.x, lwd = lwd)
}

# Wrapper around specplot. Used by the tcltk interface.
plot_spectrum <- function(x, cex = 1.0, plot = TRUE, rgb = TRUE, ...)
   specplot(x, cex = cex, plot = plot, rgb = rgb, ...)

# Wrapper around hclplot. Used by the tcltk interface.
plot_hclplot <- function(x, cex = 1.0, ...)
   hclplot(x, cex = cex, ...)


