#' Palette Swatch Plot
#' 
#' Visualization of color palettes in columns of color swatches.
#' 
#' The function \code{swatchplot} is a convenience function for displaying
#' collections of palettes that can be specified as lists or matrices of
#' character color specifications. Essentially, the function just calls
#' \code{\link[graphics]{rect}} but the value-added are the heuristics used
#' for choosing default labels, margins, spacings, borders. These are selected
#' to work well for \code{\link{hcl_palettes}} and might need further tweaking
#' in future versions.
#' 
#' @param x character vector/matrix (or list of character vectors/matrices)
#' containing color hex codes.
#' @param \dots further (possibly named) character vectors/matrices with color
#' hex codes.
#' @param nrow integer specifying the maximal number of rows of swatches.
#' (The actual number might be lower in order to balance the rows used in each column.)
#' @param border color for border of individual color rectangles. By default
#' \code{"lightgray"} for up to 9 colors, \code{"transparent"} otherwise.
#' @param sborder color for border of the entire palette swatch. By default
#' \code{"lightgray"} if \code{border} is \code{"transparent"} and \code{"lightgray"}
#' otherwise (if \code{off = 0}).
#' @param off numeric vector of length 2. Offset in horizontal and vertical direction
#' (specified as a fraction of the rectangle for one color). By default, the
#' horizontal offset is \code{0.3} for up to 5 colors and \code{0} otherwise,
#' and the vertical offset is \code{0.1}.
#' @param mar numeric vector of length 4, specifying the margins of column
#' of color swatches.
#' @param line numeric. Line in which the palette names (if any) are printed
#' in the margin.
#' @param cex,font numeric vectors of length 1 or 2. Specifications for the
#' annotation text for the individual palettes and lists of palettes, respectively.
#' @return \code{swatchplot} invisibly returns a matrix with colors and annotations.
#' @references Zeileis A, Fisher JC, Hornik K, Ihaka R, McWhite CD, Murrell P, Stauffer R, Wilke CO (2019).
#' \dQuote{ccolorspace: A Toolbox for Manipulating and Assessing Colors and Palettes.}
#' arXiv:1903.06490, arXiv.org E-Print Archive. \url{http://arxiv.org/abs/1903.06490}
#' @keywords hplot
#' @examples
#' ## swatches of several palette vectors
#' swatchplot(
#'   "Hue"       = sequential_hcl(5, h = c(0, 300), c = c(60, 60), l = 65),
#'   "Chroma"    = sequential_hcl(5, h = 0, c = c(100, 0), l = 65, rev = TRUE, power = 1),
#'   "Luminance" = sequential_hcl(5, h = 260, c = c(25, 25), l = c(25, 90), rev = TRUE, power = 1),
#'   off = 0
#' )
#'
#' ## swatches of named palette matrices
#' bprg <- c("Blues", "Purples", "Reds", "Greens")
#' swatchplot(
#'   "Single-hue"            = t(sapply(paste(bprg, 2), sequential_hcl, n = 7)),
#'   "Single-hue (advanced)" = t(sapply(paste(bprg, 3), sequential_hcl, n = 7)),
#'   "Multi-hue (advanced)"  = t(sapply(bprg,           sequential_hcl, n = 7)),
#'   nrow = 5
#' )
#' @export swatchplot
#' @importFrom graphics rect mtext par plot
swatchplot <- function(x, ..., nrow = 20, border = NULL, sborder = NULL, off = NULL,
  mar = NULL, line = NULL, cex = NULL, font = 1:2)
{
  ## canonicalize specification to list of vectors/matrices
  x <- if(missing(x)) {
    list()
  } else if(is.data.frame(x)) {
    list(as.matrix(x))
  } else if(is.matrix(x)) {
    list(x)
  } else if(!is.list(x)) {
    list(x)
  } else {
    x
  }
  if(!missing(...)) {
    x <- c(x, list(...))
  }
  
  ## expand to maximum number of palettes
  n <- max(sapply(x, function(y) {
    if(is.null(dim(y))) length(y) else ncol(y)
  }))
  x <- lapply(x, function(y) {
    if(!is.character(y)) y[] <- as.character(y)
    if(is.null(dim(y))) y <- matrix(y, ncol = length(y))
    if(NCOL(y) < n) {
      y <- cbind(y, matrix("transparent", nrow = NROW(y), ncol = n - NCOL(y)))
    }
    if(is.null(rownames(y))) rownames(y) <- rep.int("", NROW(y))
    colnames(y) <- paste("Color", 1L:n)
    return(y)
  })
  ## add extra row for names (if any)
  if(!is.null(names(x))) {
    x <- lapply(names(x), function(i) {
      if(nrow(x[[i]]) == 1L && rownames(x[[i]]) == "") {
        rownames(x[[i]]) <- i
	return(x[[i]])
      } else {
        rbind(
          matrix(rep.int("transparent", n), nrow = 1L, ncol = n,
	    dimnames = list(i, colnames(x[[i]]))),
	  x[[i]]
        )
       }
    })
  }
  
  ## transform into one big matrix
  x <- do.call("rbind", x)
  x[is.na(x)] <- "transparent"
  m <- nrow(x)
  
  ## graphical parameters
  rnam <- rownames(x)[x[,1L] != "transparent"]

  if(is.null(cex)) cex <- if(m > 15L) 0.7 else 1
  if(length(cex) < 2L) cex <- c(1, 1.4) * cex
  mcex <- rep.int(cex[1L], m)
  mcex[x[, 1L] == "transparent"] <- cex[2L]
  
  mfont <- rep.int(font[1L], m)
  mfont[x[, 1L] == "transparent"] <- font[2L]
  
  lin <- if(is.null(line)) cex[1L] * (0.5 + max(nchar(rnam) - 1L)/2.25) else line
  lin <- pmax(0.08, lin)
  
  if(m > 1L) {
    if(is.null(mar)) {
      mar <- if(any(rnam != "")) {
        c(0.1, 1.25 * lin, 0, 0)
      } else {
        c(0.1, 0.1, 0, 0)
      }
    }
    nc <- ceiling(m/nrow)
    nr <- ceiling(m/nc)
    opar <- if(nc > 1L) par(mar = mar, mfrow = c(1L, nc)) else par(mar = mar)
    on.exit(par(opar))
  } else {
    nc <- 1L
    nr <- 1L
  }
  col <- rep_len(rep(1L:nc, each = nr), length.out = m)
  
  if(is.null(off)) off <- if(n < 7L) 0.1 else 0
  if(length(off) < 2L) off <- c(off, 0.3)
  off <- c(1, 1) - off

  ## swatch border color: light gray for sufficiently "discrete" case
  if(is.null(border)) {
    border <- if(n > 9L) "transparent" else "lightgray"
  }
  border <- rep.int(border, n * m)
  border[x == "transparent"] <- "transparent"
  border <- matrix(border, ncol = n)
  ## palette border color: light gray for overall palette if off = 0 & border = "transparent"
  if(is.null(sborder)) {
    sborder <- ifelse(border[, 1L] == "transparent" & off[1L] >= 1, "lightgray", "transparent")
  }
  sborder <- rep.int(sborder, m)
  sborder[apply(x == "transparent", 1, all)] <- "transparent"
  
  ## visualization
  for(i in 1L:nc) {
    ## empty plot
    plot(0, 0, type = "n", xlim = c(0, 1 - (1 - off[1L])/n), ylim = c(0, 1 - (1 - off[2L])/nr), axes = FALSE, xlab = "", ylab = "")
    ## extract colors for i-th column
    xi <- x[col == i, , drop = FALSE]
    nri <- nrow(xi)
    ## set up x- and y-coordinates for swatches
    x1 <- rep(0:(n-1)/n, each = nri)
    y1 <- rep.int((nr-1):(nr-nri)/nr, n)
    ## annotation
    mtext(rownames(xi), side = 2, at = y1[1L:nri] + 0.5 * off[2L]/nr,
      las = 1, line = lin, adj = 0, cex = mcex[col == i], font = mfont[col == i])
    ## draw swatches
    rect(x1, y1, x1 + off[1L]/n, y1 + off[2L]/nr, col = xi, border = border[col == i])
    ## draw palette borders
    rect(0, y1[1L:nri], 1, y1[1L:nri] + off[2L]/nr, col = "transparent", border = sborder[(i - 1L) * nr + 1L:nri])
  }

  invisible(x)
}

## ## Perceptually-based color model
## ## Hue: Type of color (H = 0, ..., 300, C = 60, L = 65)
## sequential_hcl(5, h = c(0, 300), c = c(60, 60), l = 65)
## ## Chroma: Colorfulness (H = 0, C = 0, ..., 100, L = 65)
## sequential_hcl(5, h = 0, c = c(100, 0), l = 65, rev = TRUE, power = 1)
## ## Luminance: Brightness (H = 260, C = 25, L = 25, ..., 90)
## sequential_hcl(5, h = 260, c = c(25, 25), l = c(25, 90), rev = TRUE, power = 1)
