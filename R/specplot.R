# Plotting spectrum
specplot <- function(x, rgb = TRUE, hcl = TRUE, fix = TRUE, cex = 1,
  type = "l", lwd = 2 * cex, lty = 1, pch = NULL,
  legend = TRUE, palette = TRUE, plot = TRUE)
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
  # (1) as(RGB, "polarLUV") returns hue's in the
  # range of 0-360. A palette from -100 to +100 results in
  # c(260-360,0,100) - the iterative approach corrects this.

  if(fix & nrow(HCL) > 1L) {
    for(i in 2L:nrow(HCL)) {
      if ( any(is.na(HCL[(i-1L):i,])) ) next
      d <- HCL[i, "H"] - HCL[i - 1L, "H"]
      if (abs(d) > 320) HCL[i, "H"] <- HCL[i, "H"] - sign(d) * 360
      if (abs(HCL[i, "H"]) >  360) HCL[1L:i, "H"] <- HCL[1L:i, "H"] - sign(HCL[i, "H"]) * 360
    }

    # (2) Smoothing hue values in batches where chroma is very low
    idx <- which(HCL[, "C"] < 8)
    if (length(idx) == nrow(HCL)) {
      HCL[,"H"] <- mean(HCL[,"H"])
    } else if (length(idx) > 0L) {
      ## pre-smooth hue
      n <- nrow(HCL)
      if(n >= 49L) {
        HCL[, "H"] <- 1/3 * (
          HCL[c(rep.int(1L, 2L), 1L:(n - 2L)), "H"] +
          HCL[c(rep.int(1L, 1L), 1L:(n - 1L)), "H"] +
          HCL[                   1L:n,         "H"])
      }
      idxs <- split(idx, cumsum(c(1, diff(idx)) > 1))
      s <- 1L
      while(length(idxs) > 0L) {
        e <- if(s %in% idxs[[1L]]) {
          if(length(idxs) > 1L) idxs[[2L]] - 1L else n
        } else {
               if(n %in% idxs[[1L]]) n else round(mean(range(idxs[[1L]])))
        }
        io <- split(s:e, s:e %in% idx)
        if (length(io) == 2L & sum(!is.na(HCL[io[["FALSE"]],"H"])) > 0) {
          HCL[io[["TRUE"]], "H"] <- stats::spline(io[["FALSE"]], HCL[io[["FALSE"]], "H"],
            xout = io[["TRUE"]], method = "natural")$y
        }
        idxs[[1L]] <- NULL
        s <- e + 1L
      }
    }
  }

  # plot spectra and palette?
  if(isTRUE(rgb)) rgb <- hex(sRGB(c(0.8, 0, 0), c(0, 0.8, 0), c(0, 0, 0.8)))
  if(isTRUE(hcl)) hcl <- rainbow_hcl(3L)
  show_rgb <- !identical(rgb, FALSE)
  show_hcl <- !identical(hcl, FALSE)

  if(plot & (show_rgb | show_hcl) & (length(x.na) == length(x))) {
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))

    par(xaxt='n',yaxt='n',bty='n',mar=rep(0,4))
    plot(0,type='n',xlim=c(-1,1),ylim=c(-1,1))
    text(0,0,"All colors NA\nCannot draw spectrum",col=2)
  } else if(plot & (show_rgb | show_hcl)) {
    ## set up plot layout
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
    nr <- show_rgb + palette + show_hcl
    layout(matrix(1L:nr, ncol = 1L, nrow = nr),
      heights = c(if(show_rgb) 10 else NULL, if(palette) 2 else NULL, if(show_hcl) 10 else NULL))
    par(xaxt = "n", yaxs = "i", xaxs = "i",
      mar = c(0.2, 0, 0.2, 0), oma = c(2, 3, 2, 3), cex = cex)

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
      if(legend) legend("topleft", legend = c("Red", "Green", "Blue"),
        ncol = 3L, bty = "n", lwd = lwd, lty = lty, col = rgb, pch = pch)
      mtext(side = 3, "RGB Spectrum",       cex = cex, line = 0.2)
      mtext(side = 2, "Red / Green / Blue", cex = cex, line = 2.0)
    }

    # color palette
    if(palette) {
      par(xaxt = "n", yaxt = "n")
      image(matrix(seq_along(x), ncol = 1L), col = x)
      par(yaxt = "s")
    }

    # HCL spectrum
    if(show_hcl) {
      plot(0, type = "n", ylim = c(0, 100), xlim = c(1, length(x)))
      if ( min(HCL[,"H"],na.rm=TRUE) >= 0 ) {
         labels <- seq(   0, 360, length.out = 5)
         axis(side = 4, at = labels/3.6, labels = labels)
         lines((HCL[, "H"])/3.6, lwd = lwd[1L], lty = lty[1L], col = hcl[1L], type = type[1L], pch = pch[1L])
      } else {
         labels <- seq(-360, 360, length.out = 5)
         axis(side = 4, at = labels/7.2 + 50, labels = labels)
         lines((HCL[, "H"] + 360)/7.2, lwd = lwd[1L], lty = lty[1L], col = hcl[1L], type = type[1L], pch = pch[1L])
      }
      lines( HCL[, "C"],       lwd = lwd[2L], lty = lty[2L], col = hcl[2L], type = type[1L], pch = pch[1L])
      lines( HCL[, "L"],            lwd = lwd[3L], lty = lty[3L], col = hcl[3L], type = type[1L], pch = pch[1L])
      legend("bottomleft", legend = c("Hue", "Chroma", "Luminance"),
        ncol = 3L, bty = "n", lwd = lwd, lty = lty, col = hcl, pch = pch)
      mtext(side = 1, "HCL Spectrum",    cex = cex, line = 0.2)
      mtext(side = 2, "Chroma / Luminance", cex = cex, line = 2.0)
      mtext(side = 4, "Hue",       cex = cex, line = 2.0)
    }
  }

  # Return
  if ( length(x.na) > 0 ) x[x.na] <- NA
  invisible(list(
    RGB = RGB,
    HCL = HCL,
    hex = x)
  ) 
}
