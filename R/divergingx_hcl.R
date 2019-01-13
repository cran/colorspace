#' (More) Flexible Diverging HCL Palettes
#' 
#' Diverging HCL color palettes generated through combination of two fully
#' flexible (and possibly unbalanced) multi-hue sequential palettes.
#'
#' The \code{divergingx_hcl} function simply calls \code{\link{sequential_hcl}}
#' twice with a prespecified set of hue, chroma, and luminance parameters. This is
#' similar to \code{\link{diverging_hcl}} but allows for more flexibility:
#' \code{diverging_hcl} employs two \emph{single-hue} sequential palettes,
#' always uses zero chroma for the neutral/central color, and restricts the
#' chroma/luminance path to be the same in both \dQuote{arms} of the palette.
#' In contrast, \code{divergingx_hcl} relaxes this to two full \emph{multi-hue}
#' palettes that can thus go through a non-gray neutral color (typically light
#' yellow). Consequently, the chroma/luminance paths can be rather unbalanced
#' between the two arms.
#'
#' With this additional flexibility various diverging palettes suggested by
#' \url{http://ColorBrewer.org/} and CARTO (\url{https://carto.com/carto-colors/})
#' can be emulated.
#'
#' Available CARTO palettes: ArmyRose, Earth, Fall, Geyser, TealRose, Temps, and
#' Tropic (with Tropic also available in \code{diverging_hcl}).
#'
#' Available ColorBrewer.org palettes: PuOr, RdBu, RdGy, PiYG, PRGn, BrBG, RdYlBu,
#' RdYlGn, Spectral.
#' 
#' @param n the number of colors (\eqn{\ge 1}{>= 1}) to be in the palette.
#' @param palette character with the name (see details).
#' @param \dots arguments passed to \code{\link{sequential_hcl}}.
#' @param rev logical. Should the palette be reversed?
#' @param h1 numeric. Starting hue coordinate.
#' @param h2 numeric. Center hue coordinate.
#' @param h3 numeric. Ending hue coordinate.
#' @param c1 numeric. Chroma coordinate corresponding to \code{h1}.
#' @param c2 numeric. Chroma coordinate corresponding to \code{h2} (if \code{NA}, set to 0).
#' @param c3 numeric. Chroma coordinate corresponding to \code{h3}.
#' @param l1 numeric. Luminance coordinate corresponding to \code{h1}.
#' @param l2 numeric. Luminance coordinate corresponding to \code{h2}.
#' @param l3 numeric. Luminance coordinate corresponding to \code{h3} (if \code{NA}, \code{l1} is used).
#' @param p1 numeric. Power parameter for chroma coordinates in first sequential palette.
#' @param p2 numeric. Power parameter for luminance coordinates in first sequential palette (if \code{NA}, \code{p1} is used).
#' @param p3 numeric. Power parameter for chroma coordinates in second sequential palette (if \code{NA}, \code{p1} is used).
#' @param p4 numeric. Power parameter for luminance coordinates in second sequential palette (if \code{NA}, \code{p3} is used).
#' @param cmax1 numeric. Maximum chroma coordinate in first sequential palette (not used if \code{NA}).
#' @param cmax2 numeric. Maximum chroma coordinate in second sequential palette (if \code{NA}, \code{cmax1} is used).
#' @param plot logical. Should the selected HCL color palettes be visualized?
#'
#' @return A character vector with (s)RGB codings of the colors in the palette.
#' @seealso \code{\link[colorspace]{sequential_hcl}}, \code{\link[colorspace]{diverging_hcl}}
#' @keywords color
#' @examples
#' ## show emulated CARTO/ColorBrewer.org palettes
#' divergingx_palettes(plot = TRUE)
#'
#' ## compared to diverging_hcl() the diverging CARTO palettes are typically warmer
#' ## but also less balanced with respect to chroma/luminance, see e.g.,
#' specplot(divergingx_hcl(7, "ArmyRose"))
#' @rdname divergingx_hcl

#' @export
divergingx_hcl <- function(n, palette = "Geyser", ..., rev = FALSE,
  h1, h2, h3, c1, c2, c3, l1, l2, l3, p1, p2, p3, p4, cmax1, cmax2)
{
    ## empty palette
    if(n < 1L) return(character(0L))

    ## obtained stored coordinates    
    palette <- match.arg(palette, names(divex_pals))
    pals <- divex_pals[[palette]]
    names(pals) <- c("h1", "h2", "h3", "c1", "c2", "c3", "l1", "l2", "l3", "p1", "p2", "p3", "p4", "cmax1", "cmax2")

    ## replace coordinates (if specified)
    if(!missing(h1)) pals["h1"] <- h1
    if(!missing(h2)) pals["h2"] <- h2
    if(!missing(h3)) pals["h3"] <- h3
    if(!missing(c1)) pals["c1"] <- c1
    if(!missing(c2)) pals["c2"] <- c2
    if(!missing(c3)) pals["c3"] <- c3
    if(!missing(l1)) pals["l1"] <- l1
    if(!missing(l2)) pals["l2"] <- l2
    if(!missing(l3)) pals["l3"] <- l3
    if(!missing(p1)) pals["p1"] <- p1
    if(!missing(p2)) pals["p2"] <- p2
    if(!missing(p3)) pals["p3"] <- p3
    if(!missing(p4)) pals["p4"] <- p4
    if(!missing(cmax1)) pals["cmax1"] <- cmax1
    if(!missing(cmax2)) pals["cmax2"] <- cmax2

    ## resolve NAs
    ## first coordinate
    if(any(is.na(pals[c("h1", "c1", "l1")]))) stop("first hue/chroma/luminance coordinate must be specified")
    if(is.na(pals["p1"])) pals["p1"] <- 1
    ## second coordinate
    if(is.na(pals["c2"])) pals["c2"] <- 0
    if(is.na(pals["l2"])) pals["l2"] <- pals["l1"]
    if(is.na(pals["p2"])) pals["p2"] <- pals["p1"]
    ## third coordinate
    if(is.na(pals["h3"])) stop("third hue coordinate must be specified")
    if(is.na(pals["c3"])) pals["c3"] <- pals["c1"]
    if(is.na(pals["l3"])) pals["l3"] <- pals["l1"]
    if(is.na(pals["p3"])) pals["p3"] <- pals["p1"]
    if(is.na(pals["p4"])) pals["p4"] <- pals["p2"]

    ## call sequential_hcl() once or twice
    n2 <- ceiling(n/2)    
    rval <- sequential_hcl(n2, rev = TRUE,
    	h1 = pals["h1"], h2 = pals["h2"], c1 = pals["c1"], c2 = pals["c2"],
    	l1 = pals["l1"], l2 = pals["l2"], p1 = pals["p1"], p2 = pals["p2"],
	cmax = pals["cmax1"], ...)
    if(floor(n/2) < n2) rval <- rval[-1L]
    rval <- c(rev(rval), sequential_hcl(n2, rev = TRUE,
    	h1 = pals["h3"], h2 = pals["h2"], c1 = pals["c3"], c2 = pals["c2"],
    	l1 = pals["l3"], l2 = pals["l2"], p1 = pals["p3"], p2 = pals["p4"],
	cmax = pals["cmax2"], ...))
    if(rev) rval <- rev(rval)
    return(rval)   
}

#' @rdname divergingx_hcl
#' @usage NULL
#' @export
divergex_hcl <- divergingx_hcl

#' @rdname divergingx_hcl
#' @export
divergingx_palettes <- function(palette = NULL, plot = FALSE, n = 7L, ...)
{
  ## collect all divergingx palettex
  pals <- as.data.frame(do.call("rbind", divex_pals))
  rownames(pals) <- names(divex_pals)
  pals$type <- factor(rep.int("Diverging (flexible)", nrow(pals)))
  names(pals) <- c("h1", "h2", "h3", "c1", "c2", "c3", "l1", "l2", "l3", "p1", "p2", "p3", "p4", "cmax1", "cmax2", "type")
  pals$fixup <- TRUE
  pals <- pals[, c(16L, 1L:15L)]

  ## subset by type and name (by flexible matching)
  if(!is.null(palette)) {
    fx <- function(n) tolower(gsub("[-, _, \\,, (, ), \\ , \\.]", "", n))
    namtab <- fx(rownames(pals))
    palette <- sapply(fx(palette), function(n) {
      if(n %in% namtab) return(n)
      n <- startsWith(namtab, n)
      if(all(!n)) stop("Named 'palette' should be one of: ", paste(rownames(pals), collapse = ", "))
      namtab[which(n)[1L]]
    })
    pals <- pals[fx(rownames(pals)) %in% palette, , drop = FALSE]
  }

  ## add class and show selection
  class(pals) <- c("hcl_palettes", "data.frame")
  if(plot) {
    plot(pals, n = n, ...)
    invisible(pals)
  } else {
    return(pals)
  }
}

divex_pals <- list()

## CARTO
divex_pals[["ArmyRose"]] <- c(  0,  NA,  93,  73,  18,  47,  58,  98,  52, 1.5, 0.8, 0.8, 1.0,  NA, 55)
divex_pals[["Earth"]]    <- c( 43,  82, 221,  61,  30,  45,  50,  92,  52, 1.0, 1.0, 0.8, 1.0,  NA, 10)
divex_pals[["Fall"]]     <- c(133,  77,  21,  20,  35, 100,  35,  95,  50, 1.0,  NA, 1.5,  NA,  NA, NA)
divex_pals[["Geyser"]]   <- c(192,  77,  21,  40,  35, 100,  50,  95,  50, 1.0, 1.0, 1.2, 1.0,  20, NA)
divex_pals[["TealRose"]] <- c(190,  77,   0,  50,  25,  80,  55,  92,  55, 1.5, 1.0, 1.8, 1.0,  15, NA)
divex_pals[["Temps"]]    <- c(191,  80,  -4,  43,  50,  78,  55,  89,  54, 1.6, 1.0, 1.0, 1.0,  57, 85)
divex_pals[["Tropic"]]   <- c(195,  NA, 325,  70,  NA,  NA,  55,  95,  NA, 1.0,  NA,  NA,  NA,  NA, NA)

## ColorBrewer.org
divex_pals[["PuOr"]]     <- c( 40,  NA, 270,  70,   0,  30,  30,  98,  10, 0.6, 1.4, 1.5, 1.3, 100, 65)
divex_pals[["RdBu"]]     <- c( 20,  NA, 230,  60,   0,  50,  20,  98,  15, 1.4, 1.2, 1.5, 1.5, 125, 90)
divex_pals[["RdGy"]]     <- c(  5,  50,  50,  60,   0,   0,  20,  98,  20, 1.2, 1.2, 1.0, 1.2, 125, NA)
divex_pals[["PiYG"]]     <- c(340,  NA, 115,  75,   0,  50,  30,  98,  35, 1.3, 1.4, 0.8, 1.5, 100, 80)
divex_pals[["PRGn"]]     <- c(300,  NA, 128,  30,   0,  30,  15,  97,  25, 1.3, 1.2, 0.9, 1.5,  65, 65)
divex_pals[["BrBG"]]     <- c( 55,  NA, 180,  40,   0,  30,  25,  97,  20, 0.8, 1.4, 0.8, 1.4,  75, 45)
divex_pals[["RdYlBu"]]   <- c( 10,  85, 260, 105,  45,  70,  35,  98,  35, 1.5, 1.2, 0.6, 1.2, 150, 10)
divex_pals[["RdYlGn"]]   <- c( 10,  85, 140, 105,  45,  50,  35,  98,  35, 1.5, 1.2, 0.8, 1.2, 150, 75)
divex_pals[["Spectral"]] <- c(  0,  85, 270,  90,  45,  65,  37,  98,  37, 1.0, 1.2, 1.0, 1.2, 120, NA)
