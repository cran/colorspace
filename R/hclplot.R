#' Palette Plot in HCL Space
#' 
#' Visualization of color palettes in HCL space projections.
#' 
#' The function \code{hclplot} is an auxiliary function for illustrating
#' the trajectories of color palettes in two-dimensional HCL space projections.
#' It collapses over one of the three coordinates (either the hue H or the
#' luminance L) and displays a heatmap of colors combining the remaining
#' two dimensions. The coordinates for the given color palette are highlighted
#' to bring out its trajectory.
#' 
#' The function \code{hclplot} has been designed to work well with the
#' \code{\link{hcl_palettes}} in this package. While it is possible to apply
#' it to other color palettes as well, the results might look weird or confusing
#' if these palettes are constructed very differently (e.g., as in the highly
#' saturated base R palettes).
#'
#' More specifically, the following palettes can be visualized well: \itemize{
#'   \item Qualitative with (approximately) constant luminance. In this case,
#'      \code{hclplot} shows a hue-chroma plane (in polar coordinates), keeping
#'      luminance at a fixed level (by default displayed in the main title of
#'      the plot). If the luminance is, in fact, not approximately constant,
#'      the luminance varies along with hue and chroma, using a simple linear
#'      function (fitted by least squares).
#    \item Sequential with (approximately) constant hue. In this case,
#'      \code{hclplot} shows a chroma-luminance plane, keeping hue at a fixed
#'      level (by default displayed in the main title of the plot). If the hue
#'      is, in fact, not approximately constant, the hue varies along with
#'      chroma and luminance, using a simple linear function (fitted by least
#'      squares.
#'   \item Diverging with two (approximately) constant hues: This case is
#'      visualized with two back-to-back sequential displays.
#' }
#' To infer the type of display to use, by default, the following heuristic is
#' used: If luminance is not approximately constant (range > 10) and follows
#' rougly a triangular pattern, a diverging display is used. If luminance is
#' not constant and follows roughly a linear pattern, a sequential display is
#' used. Otherwise a qualitative display is used.
#' 
#' @param x character vector containing color hex codes, or a \code{\link{color-class}}
#' object.
#' @param type type character specifying which type of palette should be visualized
#' (\code{"qualitative"}, \code{"sequential"}, or \code{"diverging"}).
#' For qualitative palettes a hue-chroma plane is used, otherwise a chroma-luminance plane.
#' By default, the \code{type} is inferred from the luminance trajectory corresponding
#' to \code{x}.
#' @param h numeric hue(s) to be used for \code{type = "sequential"} and \code{type = "diverging"}.
#' By default, these are inferred from the colors in \code{x}.
#' @param c numeric. Maximal chroma value to be used.
#' @param l numeric luminance(s) to be used for \code{type = "qualitative"}.
#' By default, this is inferred from the colors in \code{x}.
#' @param xlab,ylab,main character strings for annotation, by default generated from
#' the type of color palette visualized.
#' @param cex numeric character extension.
#' @param axes logical. Should axes be drawn?
#' @param bg,lwd,size graphical control parameters for the color palette trajectory.
#' @param \dots currently not used.
#'
#' @return \code{hclplot} invisibly returns a matrix with the HCL coordinates corresponding to \code{x}.
#' @seealso \code{\link{specplot}}
#' @keywords hplot
#' @examples
#' ## for qualitative palettes luminance and chroma are fixed, varying only hue
#' hclplot(qualitative_hcl(9, c = 50, l = 70))
#' 
#' ## single-hue sequential palette (h = 260) with linear vs. power-transformed trajectory
#' hclplot(sequential_hcl(7, h = 260, c = 80, l = c(35, 95), power = 1))
#' hclplot(sequential_hcl(7, h = 260, c = 80, l = c(35, 95), power = 1.5))
#' 
#' ## advanced single-hue sequential palette with triangular chroma trajectory
#' ## (piecewise linear vs. power-transformed)
#' hclplot(sequential_hcl(7, h = 245, c = c(40, 75, 0), l = c(30, 95), power = 1))
#' hclplot(sequential_hcl(7, h = 245, c = c(40, 75, 0), l = c(30, 95), power = c(0.8, 1.4)))
#' 
#' ## multi-hue sequential palette with small hue range and triangular chroma vs.
#' ## large hue range and linear chroma trajectory
#' hclplot(sequential_hcl(7, h = c(260, 220), c = c(50, 75, 0), l = c(30, 95), power = 1))
#' hclplot(sequential_hcl(7, h = c(260, 60), c = 60, l = c(40, 95), power = 1))
#' 
#' ## balanced diverging palette constructed from two simple single-hue sequential
#' ## palettes (for hues 260/blue and 0/red)
#' hclplot(diverging_hcl(7, h = c(260, 0), c = 80, l = c(35, 95), power = 1))
#' 
#' @export hclplot
#' @importFrom graphics box lines mtext par plot points rect text
#' @importFrom stats cor lm median predict
hclplot <- function(x, type = NULL, h = NULL, c = NULL, l = NULL,
    xlab = NULL, ylab = NULL, main = NULL, cex = 1.0, axes = TRUE,
    bg = "white", lwd = 1, size = 2.5, ...)
{  

    ## TODO: Not yet able to handle NA values. Thus, replace
    ## NA values with white, if needed.
    NAidx <- which(is.na(x)); if (length(NAidx) > 0) x[NAidx] <- "#FFFFFF"
    ## convert to HCL coordinates
    if(is.character(x)) {
      HCL <- hex2RGB(x)
    } else {
      HCL <- x
      x <- hex(x)
    }
    HCL <- coords(as(HCL, "polarLUV"))[, c("H", "C", "L")]
    n <- nrow(HCL)

    ## determine type of palette based on luminance trajectory
    lran <- diff(range(HCL[, "L"], na.rm = TRUE))
    llin <- cor(HCL[, "L"], 1L:n, use = "pairwise.complete.obs")^2
    ltri <- cor(HCL[, "L"], abs(1L:n - (n + 1)/2), use = "pairwise.complete.obs")^2
    if(is.null(type)) {
      type <- if(ltri > 0.75 & lran > 10) {
        "diverging"
      } else if(llin > 0.75 & lran > 10) {
        "sequential"
      } else {
        "qualitative"
      }
    } else {
      type <- match.arg(type, c("diverging", "sequential", "qualitative"))
    }

    ## FIXME: put into separate function
    if(n > 1L) {
        for(i in 2L:n) {
            if ( any(is.na(HCL[(i-1L):i,])) ) next
            d <- HCL[i, "H"] - HCL[i - 1L, "H"]
            if (abs(d) > 320) HCL[i, "H"] <- HCL[i, "H"] - sign(d) * 360
            if (abs(HCL[i, "H"]) >  360) HCL[1L:i, "H"] <- HCL[1L:i, "H"] - sign(HCL[i, "H"]) * 360
        }

        # (2) Smoothing hue values in batches where chroma is very low
        idx <- which(HCL[, "C"] < 8)
        if (length(idx) == n) {
            HCL[,"H"] <- mean(HCL[,"H"])
        } else if (length(idx) > 0L) {
            ## pre-smooth hue
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

    maxchroma <- if(!is.null(c)) ceiling(c) else pmax(100, pmin(180, ceiling(max(HCL[, "C"], na.rm = TRUE)/20) * 20))

    switch(type,
        "sequential" = {
            opar <- par(cex = cex, mar = c(3, 3, 2, 1) * cex, no.readonly = TRUE)
            on.exit(par(opar))
            nd <- expand.grid(C = 0:maxchroma, L = 0:100)
            if(!is.null(h)) {
                nd$H <- h
            } else if(n < 3L || diff(range(HCL[, "H"], na.rm = TRUE)) < 12) {
                nd$H <- median(HCL[, "H"], na.rm = TRUE)
            } else {
                m <- lm(H ~ C + L, data = as.data.frame(HCL))
		sig <- summary(m)$sigma
                if(is.na(sig) || sig > 7.5) warning("cannot approximate H well as a linear function of C and L")
                nd$H <- predict(m, nd)
            }
            if(is.null(main)) {
                main <- if(length(unique(nd$H)) <= 1L) {
                    round(nd$H[1L])
                } else {
                    paste("[", round(min(nd$H, na.rm = TRUE)), ", ", round(max(nd$H, na.rm = TRUE)), "]", sep = "")
                }
                main <- paste("Hue =", main)
            }
            HCL2 <- hex(polarLUV(H = nd$H, C = nd$C, L = nd$L), fixup = FALSE)
            HCL2[nd$L < 1 & nd$C > 0] <- NA
            plot(0, 0, type = "n", xlim = c(0, maxchroma), ylim = c(0, 100), xaxs = "i", yaxs = "i",
                 xlab = NA, ylab = NA, main = main, axes = axes)
            # Adding axis labels
            if(axes) {
                if ( is.null(xlab) ) xlab <- "Chroma"
                if ( is.null(ylab) ) ylab <- "Luminance"
                mtext(side = 1, line = 2 * cex, xlab, cex = cex)
                mtext(side = 2, line = 2 * cex, ylab, cex = cex)
            }
            # Adding colors
            points(nd$C, nd$L, col = HCL2, pch = 19, cex = 3)
            points(HCL[, 2L:3L], pch = 19, cex = 1.1 * size * cex,  type = "p", lwd = 5 * lwd, col = bg)
            points(HCL[, 2L:3L], pch = 21, bg = x, cex = size * cex, type = "o", lwd = lwd)
            box()
        },
        "diverging" = {
            opar <- par(cex = cex, mar = c(3, 3, 2, 1) * cex, no.readonly = TRUE)
            on.exit(par(opar))
            nd <- expand.grid(C = -maxchroma:maxchroma, L = 0:100)
            nd$H <- NA
            nd$left <- nd$C < 0
            left  <- 1L:floor(n/2)
            left  <- left[HCL[left, "C"] > 10]
            right <- ceiling(n/2):n
            right <- right[HCL[right, "C"] > 10]
        
            if(!is.null(h)) {
                if(length(h) == 2L) {
                    nd$H[nd$left]  <- h[1L]
                    nd$H[!nd$left] <- h[2L]
                } else {
                    nd$H <- h
                }
            } else if(n < 6L || (diff(range(HCL[left, "H"]  - min(HCL[ left, "H"], na.rm = TRUE), na.rm = TRUE)) < 12 &
                                 diff(range(HCL[right, "H"] - min(HCL[right, "H"], na.rm = TRUE), na.rm = TRUE)) < 12)) {
                nd$H[nd$left]  <- median(HCL[ left, "H"] - min(HCL[ left, "H"], na.rm = TRUE), na.rm = TRUE) + min(HCL[ left, "H"], na.rm = TRUE)
                nd$H[!nd$left] <- median(HCL[right, "H"] - min(HCL[right, "H"], na.rm = TRUE), na.rm = TRUE) + min(HCL[right, "H"], na.rm = TRUE)
            } else {
                HCLdata <- as.data.frame(HCL)
                HCLdata$left <- factor(rep(c(TRUE, FALSE), c(floor(n/2), ceiling(n/2))))
                nd$left <- factor(nd$left)
                m <- lm(H ~ left * (C + L), data = HCLdata)
		sig <- summary(m)$sigma
                if(is.na(sig) || sig > 7.5) warning("cannot approximate H well as a linear function of C and L")
                nd$H <- predict(m, nd)
                nd$left <- nd$left == "TRUE"
            }
            if(is.null(main)) {
                main <- if(length(unique(nd$H)) <= 2L) {
                    paste(round(nd$H[nd$left][1L]), "/", round(nd$H[!nd$left][1L]))
                } else {
                   paste("[",
                       round(min(nd$H[nd$left], na.rm = TRUE)), ", ", round(max(nd$H[nd$left], na.rm = TRUE)), "] / [",
                           round(min(nd$H[!nd$left], na.rm = TRUE)), ", ", round(max(nd$H[!nd$left], na.rm = TRUE)), "]", sep = "")
                }
                main <- paste("Hue =", main)
            }
            HCL2 <- hex(polarLUV(H = nd$H, C = abs(nd$C), L = nd$L), fixup = FALSE)
            HCL2[nd$L < 1 & abs(nd$C) > 0] <- NA
            plot(0, 0, type = "n", xlim = c(-1, 1) * maxchroma, ylim = c(0, 100), xaxs = "i", yaxs = "i",
                 xlab = NA, ylab = NA, main = main, axes = FALSE)
            # Axis labels
            if(axes) {
                if ( is.null(xlab) ) xlab <- "Chroma"
                if ( is.null(ylab) ) ylab <- "Luminance"
                mtext(side = 1, line = 2 * cex, xlab, cex = cex)
                mtext(side = 2, line = 2 * cex, ylab, cex = cex)
                at1 <- pretty(c(-1, 1) * maxchroma)
                axis(1, at = at1, labels = abs(at1))
                axis(2)
            }
            # Plotting colors
            points(nd$C, nd$L, col = HCL2, pch = 19, cex = 3)
            points( HCL[, "C"] * ifelse(1L:n <= floor(mean(n/2)), -1, 1),
                    HCL[, "L"], pch = 19, cex = 1.1 * size * cex,  type = "p", lwd = 5 * lwd, col = bg)
            points( HCL[, "C"] * ifelse(1L:n <= floor(mean(n/2)),-1,1),
                    HCL[, "L"], pch = 21, bg = x, cex = size * cex, type = "o", lwd = lwd)
            box()
        },
        "qualitative" = {
            opar <- par(cex = cex, mar = c(1, 1, 2, 1) * cex, bty = "n", no.readonly = TRUE)
            on.exit(par(opar))
            nd <- expand.grid(H = 0:180 * 2, C = 0:maxchroma)

            if(!is.null(l)) {
                nd$L <- l
            } else if(n < 3L || diff(range(HCL[, "L"], na.rm = TRUE)) < 10) {
                nd$L <- median(HCL[, "L"], na.rm = TRUE)
            } else {
                m <- lm(L ~ C + H, data = as.data.frame(HCL))
		sig <- summary(m)$sigma
                if(is.na(sig) || sig > 7.5) warning("cannot approximate L well as a linear function of H and C")
                nd$L <- predict(m, nd)
                nd$L <- pmin(100, pmax(0, nd$L))
            }
            if(is.null(main)) {
               main <- if(length(unique(nd$L)) <= 1L) {
                  round(nd$L[1L])
               } else {
                  paste("[", round(min(nd$L, na.rm = TRUE)), ", ", round(max(nd$L, na.rm = TRUE)), "]", sep = "")
               }
               main <- paste("Luminance =", main)
            }
            HCL2 <- hex(polarLUV(H = nd$H, C = nd$C, L = nd$L), fixup = FALSE)
            HCL2[nd$L < 1 & nd$C > 0] <- NA

            # fact: used for scaling
            fact <- 1.1 + (cex - 1) / 10
            plot(0, 0, type = "n", axes = FALSE, xlab = NA, ylab = NA, main = main,
                 xlim = c(-maxchroma, maxchroma) * fact, ylim = c(-maxchroma, maxchroma) * fact, asp = 1)
            xpos <- function(h, c) cos(h * pi/180) * c
            ypos <- function(h, c) sin(h * pi/180) * c
            points(xpos(nd$H, nd$C), ypos(nd$H, nd$C), col = HCL2, pch = 19, cex = 3)
            lines(xpos(0:360, maxchroma), ypos(0:360, maxchroma))
            
            if(axes) {
                if(is.null(xlab)) xlab <- "Chroma"
                if(is.null(ylab)) ylab <- "Hue"
                at.c <- if(maxchroma >= 150) 0:3 * 50 else 0:3 * 25
                at.h <- 0:6 * 60
                lines(c(0, maxchroma), c(0, 0))
                text(at.c, rep(-7, length(at.c)), at.c)
                text(50, -14, xlab)
                rect(at.c, 0, at.c, -3)
                if(0 %in% at.h | 360 %in% at.h) {
                  lines(xpos(0, maxchroma + c(0, 3)), ypos(0, maxchroma + c(0, 3)))
                  text(xpos(0, maxchroma + 7), ypos(0, maxchroma + 7), 0, pos = 3)
                  text(xpos(0, maxchroma + 7), ypos(0, maxchroma + 7), 360, pos = 1)
                  text(xpos(0, maxchroma + 16), ypos(0, maxchroma + 16), ylab)
                }
                at.h <- at.h[at.h > 0 & at.h < 360]
                for(hue in at.h) {
                  text(xpos(hue, maxchroma + 7), ypos(hue, maxchroma + 7), hue)
                  lines(xpos(hue, maxchroma + c(0, 3)), ypos(hue, maxchroma + c(0, 3)))
                }
            }
            points(xpos(HCL[, "H"], HCL[, "C"]), ypos(HCL[, "H"], HCL[, "C"]),
                   pch = 19, cex = 1.1 * size * cex,  type = "p", lwd = 5 * lwd, col = bg)
            points(xpos(HCL[, "H"], HCL[, "C"]), ypos(HCL[, "H"], HCL[, "C"]),
                   pch = 21, bg = x, cex = size * cex, type = "o", lwd = lwd)
            box()
        }
    )

    invisible(HCL)
}
