#' Color Spectrum Plot
#' 
#' Visualization of color palettes (given as hex codes) in HCL and/or RGB
#' coordinates.
#' 
#' The function \code{specplot} transforms a given color palette in hex codes
#' into their HCL (\code{\link[colorspace]{polarLUV}}) and/or RGB
#' (\code{\link[colorspace]{sRGB}}) coordinates. As the hues for low-chroma
#' colors are not (or poorly) identified, by default a smoothing is applied to
#' the hues (\code{fix = TRUE}). Also, to avoid jumps from 0 to 360 or vice
#' versa, the hue coordinates are shifted suitably.
#' 
#' By default (\code{plot = TRUE}), the resulting HCL and optionally RGB coordinates are
#' visualized by simple line plots along with the color palette \code{x}
#' itself. The x-axis simply gives the ordering of the colors in the palette
#' The y-axis depicts the following information: (1) Hue is drawn in red and
#' coordinates are indicated on the axis on the right with range [0, 360] or
#' (if necessary) [-360, 360]. (2) Chroma is drawn in green with coordinates on
#' the left axis. The range [0, 100] is used unless the palette necessitates
#' higher chroma values. (3) Luminance is drawn in blue with coordinates on the
#' left axis in the range [0, 100]. Luminance (and hence also chroma) is on
#' the left axis because it is arguably most important for understanding the
#' type of palette (qualitative vs. sequential vs. diverging). To facilitate
#' reading the legend the reversed order Luminance / Chroma / Hue is used so that
#' the legend labels are closer to the axis they pertain to.
#' 
#' For comparing two palettes, \code{specplot(x, y)} can be used which adds
#' lines (dashed, by default) corresponding to the \code{y} palette HCL/RGB
#' coordinates in the display.
#' 
#' @param x character vector containing color hex codes.
#' @param y optional second character vector containing further color hex codes,
#' to be used for comparing two palettes (\code{x} vs. \code{y}).
#' @param rgb logical or color specification. Should the RGB spectrum be
#' visualized? Can also be a vector of three colors for the legend of R/G/B coordinates.
#' @param hcl logical or color specification. Should the HCL spectrum be
#' visualized? Can also be a vector of three colors for the legend of H/C/L coordinates.
#' @param fix logical. Should the hues be fixed to be on a smooth(er) curve?
#' For details see below.
#' @param cex numeric. Character extension for figure axes and labels.
#' @param type,lwd,lty,pch plotting parameters passed to
#' \code{\link[graphics]{lines}} for drawing the RGB and HCL coordinates,
#' respectively. Can be vectors of length 3.
#' @param mar,oma numeric or logical. Either numeric vectors of length 4 giving
#' the (outer) margins or a logical indicating whether \code{mar}/\code{oma}
#' should be set.
#' @param main character. Main title of the plot.
#' @param legend logical. Should legends for the coordinates be plotted?
#' @param palette logical. Should the given palette \code{x} be plotted?
#' @param plot logical. Should the RGB and/or HCL coordinates be plotted?
#' @param \dots currently not used.
#' @return \code{specplot} invisibly returns a list with components
#' \item{HCL}{a matrix of HCL coordinates,}
#' \item{RGB}{a matrix of sRGB coordinates,}
#' \item{hex}{original color palette \code{x}.}
#' @author Reto Stauffer, Achim Zeileis
#' @seealso \code{\link{hcl_palettes}}, \code{\link{hclplot}}
#' @references Zeileis A, Hornik K, Murrell P (2009).  Escaping RGBland:
#' Selecting Colors for Statistical Graphics.  \emph{Computational Statistics &
#' Data Analysis}, \bold{53}, 3259--3270.
#' \doi{10.1016/j.csda.2008.11.033}
#' Preprint available from
#' \url{https://www.zeileis.org/papers/Zeileis+Hornik+Murrell-2009.pdf}.
#' 
#' Stauffer R, Mayr GJ, Dabernig M, Zeileis A (2015).  Somewhere over the
#' Rainbow: How to Make Effective Use of Colors in Meteorological
#' Visualizations.  \emph{Bulletin of the American Meteorological Society},
#' \bold{96}(2), 203--216.
#' \doi{10.1175/BAMS-D-13-00155.1}
#'
#' Zeileis A, Fisher JC, Hornik K, Ihaka R, McWhite CD, Murrell P, Stauffer R, Wilke CO (2020).
#' \dQuote{colorspace: A Toolbox for Manipulating and Assessing Colors and Palettes.}
#' \emph{Journal of Statistical Software}, \bold{96}(1), 1--49. \doi{10.18637/jss.v096.i01}
#' @keywords hplot
#' @examples
#' ## spectrum of the (in)famous RGB rainbow palette (in both RGB and HCL)
#' specplot(rainbow(100), rgb = TRUE)
#' 
#' ## spectrum of HCL-based palettes: qualitative/sequential/diverging
#' specplot(qualitative_hcl(100, "Set 2"))
#' specplot(sequential_hcl(100, "Blues 2"))
#' specplot(diverging_hcl(100, "Blue-Red"))
#' 
#' ## return computed RGB and HCL coordinates
#' res <- specplot(rainbow(10), plot = FALSE)
#' print(res)
#' @export specplot
#' @importFrom graphics axis image layout legend lines mtext text par plot
#' @importFrom stats median sd
specplot <- function(x, y = NULL, rgb = FALSE, hcl = TRUE, fix = TRUE, cex = 1,
  type = "l", lwd = 2 * cex, lty = 1, pch = NULL, mar = NULL, oma = NULL,
  main = NULL, legend = TRUE, palette = TRUE, plot = TRUE, ...)
{

  # Replace NA x with white, required for hex2RGB.
  # Store indizes of NA x to x.na for further
  # processing.
  x.na <- which(is.na(x))
  if (length(x.na) > 0) x[x.na] <- "#ffffff"
  RGB <- hex2RGB(x)
  HCL <- as(RGB, "polarLUV")

  # Replace coordinates of NA x with NA
  RGB <- coords(RGB)
  HCL <- coords(HCL)[, c("H", "C", "L")]
  HCL[which(is.na(HCL), arr.ind = TRUE)] <- 0
  if (length(x.na) > 0L) {
     for (i in 1:3) {
       HCL[x.na, i] <- NA
       RGB[x.na, i] <- NA
     }
  }

  # Fixing hue paths
  if(fix & nrow(HCL) > 1L) {
    # (1) as(RGB, "polarLUV") returns hue's in the
    # range of 0-360. A palette from -100 to +100 results in
    # c(260-360,0,100) - the iterative approach corrects this.

    ## strategy:
    ## change H by +360 or -360 if the hue trajectory has a very large jump,
    ## (i.e., difference > +/- 300) or if the trajectory is essentially linear
    ## but has a jump by +360 or -360 in additional to the "usual" difference
    ## (i.e., median difference +/- 360)
    md <- median(HCL[2L:nrow(HCL), "H"] - HCL[1L:(nrow(HCL) - 1L), "H"], na.rm = TRUE)
    for(i in 2L:nrow(HCL)) {
      if ( any(is.na(HCL[(i-1L):i,])) ) next
      d <- HCL[i, "H"] - HCL[i - 1L, "H"]
      if (abs(d) > 300 || (abs(d + 360 - md) < 4 || abs(d - 360 - md) < 4) ) HCL[i, "H"] <- HCL[i, "H"] - sign(d) * 360
      if (abs(HCL[i, "H"]) >  360) HCL[1L:i, "H"] <- HCL[1L:i, "H"] - sign(HCL[i, "H"]) * 360
    }

    # (2) Smoothing hue values in batches where chroma is very low (ad hoc: < 8)
    idx <- which(HCL[, "C"] < 8)
    if (length(idx) == nrow(HCL)) {
      HCL[, "H"] <- mean(HCL[, "H"], na.rm = TRUE)
    } else if (length(idx) > 0L) {
      # (a) pre-fixing hues where luminance is < 2 | > 98
      ix <- split(1:nrow(HCL), HCL[,"L"] < 2 | HCL[,"L"] > 98)
      if ( length(ix) == 2L ) {
        fun_fix_H <- function(HCL, ix, i) HCL[which.min(abs(ix["FALSE"][[1L]] - i)), "H"]
        HCL[ix["TRUE"][[1L]]] <- sapply(ix["TRUE"][[1L]], fun_fix_H, HCL = HCL, ix = ix)
      }
      ## (b) pre-smooth hue by rolling mean of length 3
      n <- nrow(HCL)
      if(n >= 49L) {
        HCL[, "H"] <- rowMeans(cbind(
          HCL[c(rep.int(1L, 2L), 1L:(n - 2L)), "H"],
          HCL[c(rep.int(1L, 1L), 1L:(n - 1L)), "H"],
          HCL[                   1L:n,         "H"]), na.rm = TRUE)
        HCL[x.na, "H"] <- NA
      }
      ## (c) interpolate linearly or by natural spline
      
      ## index list of consecutive segments with low-chroma colors
      idxs <- split(idx, cumsum(c(1, diff(idx)) > 1))

      ## find start "s" and end "e" for each consecutive segment
      ## (= problematic low-chroma indexes plus adjacent ok-chroma indexes)
      ## distinguish: one vs. more problematic segments
      ## and: problems only at beginning or end vs. in the middle (diverging)
      s <- 1L
      while(length(idxs) > 0L) {
        e <- if(any(s %in% idxs[[1L]])) {
          if(length(idxs) > 1L) idxs[[2L]] - 1L else n
        } else {
          if(n %in% idxs[[1L]]) n else round(mean(range(idxs[[1L]])))
        }
        ## "in" vs. "out" of low-chroma in current segment
        io <- split(min(s):max(e), (min(s):max(e) %in% idx) |  (min(s):max(e) %in% x.na))
      
        ## given enough ok-chroma observations fit a curve
        ## - either linear if residual standard error is small enough
        ## - or natural spline
        if (length(io) == 2L & sum(!is.na(HCL[io[["FALSE"]], "H"])) > 0 & length(io[["FALSE"]]) > 1) {
           linfit <- stats::lm.fit(cbind(1, io[["FALSE"]]), HCL[io[["FALSE"]], "H"])
           HCL[io[["TRUE"]], "H"] <- if(sd(linfit$residuals) < 1.3) {
                 drop(cbind(1, io[["TRUE"]]) %*% linfit$coefficients)      
           } else {
                 stats::spline(io[["FALSE"]], HCL[io[["FALSE"]], "H"],
                   xout = io[["TRUE"]], method = "natural")$y
           }
           HCL[x.na, "H"] <- NA
        }
        idxs[[1L]] <- NULL
        s <- e + 1L
      }
    }
  }

  # plot spectra and palette?
  if(isTRUE(rgb)) rgb <- hex(sRGB(c(0.8, 0, 0), c(0, 0.8, 0), c(0, 0, 0.8)))
  if(isTRUE(hcl)) hcl <- qualitative_hcl(3L)
  show_rgb <- !identical(rgb, FALSE)
  show_hcl <- !identical(hcl, FALSE)

  if(plot & (length(x.na) == length(x))) {
    opar <- par(no.readonly = TRUE)
    nam <- names(par(xaxt = "n", yaxt = "n", bty = "n", mar = rep(0, 4)))
    on.exit(par(opar[nam]))
    
    plot(0, type = "n", xlim = c(-1, 1), ylim = c(-1, 1))
    text(0, 0, "All colors NA\nCannot draw spectrum", col = 2)
  } else if(plot) {
    ## add second palette as a reference?
    if(!is.null(y)) {
      yspec <- specplot(y, rgb = rgb, hcl = hcl, fix = fix, plot = FALSE)
      y <- TRUE
    } else {
      yspec <- NULL
      y <- FALSE
    }
  
    ## set up graphical parameters and plot layout
    nr <- show_rgb + palette + (palette && y) + show_hcl
    opar <- par(no.readonly = TRUE)
    if(identical(mar, FALSE)) opar$mar <- NULL
    if(identical(oma, FALSE)) opar$oma <- NULL
    if(nr < 2L) opar$mfrow <- opar$mfcol <- opar$mfg <- NULL
    on.exit(par(opar))
    if(nr > 1L) {
      layout(matrix(1L:nr, ncol = 1L, nrow = nr),
        heights = c(if(show_rgb) 10 else NULL, if(palette) 2 else NULL, if(palette && y) 2 else NULL, if(show_hcl) 10 else NULL))
    }
    par(xaxt = "n", xaxs = "i", yaxs = "i", cex = cex)
    if(!identical(mar, FALSE)) {
      if(is.null(mar) || isTRUE(mar)) mar <- c(0.2, 0, 0.2, 0)
      par(mar = mar)
    }
    if(!identical(oma, FALSE)) {
      if(is.null(oma) || isTRUE(oma)) oma <- c(2, 3, 2 + !is.null(main), 3)
      par(oma = oma)
    }

    ## expand plotting parameters
    rgb <- rep(rgb, length.out = 3L)
    hcl <- rep(hcl, length.out = 3L)
    lwd <- rep(lwd, length.out = 3L)
    lty <- rep(lty, length.out = 3L)
    type<- rep(type,length.out = 3L)
    pch <- if(is.null(pch)) ifelse(type == "l", NA, 1) else rep(pch, length.out = 3L)

    # RGB spectrum
    if(show_rgb) {
      plot(0, type = "n", ylim = c(0, 1), xlim = c(1, length(x)))
      lines(RGB[, "R"], lwd = lwd[1L], lty = lty[1L], col = rgb[1L], type = type[1L], pch = pch[1L])
      lines(RGB[, "G"], lwd = lwd[2L], lty = lty[2L], col = rgb[2L], type = type[1L], pch = pch[1L])
      lines(RGB[, "B"], lwd = lwd[3L], lty = lty[3L], col = rgb[3L], type = type[1L], pch = pch[1L])
      if(y) {
        lines(yspec$RGB[, "R"], lwd = lwd[1L], lty = lty[1L] + 1L, col = rgb[1L], type = type[1L], pch = pch[1L])
        lines(yspec$RGB[, "G"], lwd = lwd[2L], lty = lty[2L] + 1L, col = rgb[2L], type = type[1L], pch = pch[1L])
        lines(yspec$RGB[, "B"], lwd = lwd[3L], lty = lty[3L] + 1L, col = rgb[3L], type = type[1L], pch = pch[1L])      
      }
      if(legend) legend("topleft", legend = c("Red", "Green", "Blue"),
        ncol = 3L, bty = "n", lwd = lwd, lty = lty, col = rgb, pch = pch)
      mtext(side = 3, "RGB Spectrum",       cex = cex, line = 0.2)
      mtext(side = 2, "Red / Green / Blue", cex = cex, line = 2.0)
      if(!is.null(main)) {
        mtext(side = 3, main, line = 1.5, cex = 1.5 * cex)
    main <- NULL
      }
    }

    # color palette
    if(palette) {
      par(xaxt = "n", yaxt = "n")
      image(matrix(seq_along(x), ncol = 1L), col = x)
      par(yaxt = "s")
      if(!is.null(main)) {
        mtext(side = 3, main, line = 1.0, cex = 1.5 * cex)
    main <- NULL
      }
    }
    if(palette && y) {
      par(xaxt = "n", yaxt = "n")
      image(matrix(seq_along(yspec$hex), ncol = 1L), col = yspec$hex)
      par(yaxt = "s")
    }

    # HCL spectrum
    if(show_hcl) {
      ymax <- pmax(max(HCL[, "C"], na.rm = TRUE) * 1.005, 100)
      yrad <- 360/ymax
      plot(0, type = "n", ylim = c(0, ymax), xlim = c(1, length(x)))
      if ( min(HCL[,"H"], na.rm = TRUE) >= -1 ) {
         labels <- seq(   0, 360, length.out = 5)
         axis(side = 4, at = labels/yrad, labels = labels)
         lines((HCL[, "H"])/yrad, lwd = lwd[1L], lty = lty[1L], col = hcl[1L], type = type[1L], pch = pch[1L])
     if(y) lines((yspec$HCL[, "H"])/yrad, lwd = lwd[1L], lty = lty[1L] + 1L, col = hcl[1L], type = type[1L], pch = pch[1L])
      } else {
         labels <- seq(-360, 360, length.out = 5)
         axis(side = 4, at = labels/(2 * yrad) + ymax/2, labels = labels)
         lines((HCL[, "H"] + 360)/(2 * yrad), lwd = lwd[1L], lty = lty[1L], col = hcl[1L], type = type[1L], pch = pch[1L])
         if(y) lines((yspec$HCL[, "H"] + 360)/(2 * yrad), lwd = lwd[1L], lty = lty[1L] + 1L, col = hcl[1L], type = type[1L], pch = pch[1L])
      }
      lines(HCL[, "C"], lwd = lwd[2L], lty = lty[2L], col = hcl[2L], type = type[1L], pch = pch[1L])
      lines(HCL[, "L"], lwd = lwd[3L], lty = lty[3L], col = hcl[3L], type = type[1L], pch = pch[1L])
      if(y) {
        lines(yspec$HCL[, "C"], lwd = lwd[2L], lty = lty[2L] + 1L, col = hcl[2L], type = type[1L], pch = pch[1L])
        lines(yspec$HCL[, "L"], lwd = lwd[3L], lty = lty[3L] + 1L, col = hcl[3L], type = type[1L], pch = pch[1L])
      }
      legend("bottomleft", legend = rev(c("Hue", "Chroma", "Luminance")),
        ncol = 3L, bty = "n", lwd = rev(lwd), lty = rev(lty), col = rev(hcl), pch = rev(pch))
      mtext(side = 1, "HCL Spectrum",    cex = cex, line = 0.2)
      mtext(side = 2, "Luminance / Chroma", cex = cex, line = 2.0)
      mtext(side = 4, "Hue",       cex = cex, line = 2.0)
      if(!is.null(main)) mtext(side = 3, main, line = 1.0, cex = 1.5 * cex)
    }

  }

  # Return
  if ( length(x.na) > 0 ) x[x.na] <- NA
  invisible(list(
    HCL = HCL,
    RGB = RGB,
    hex = x)
  ) 
}
