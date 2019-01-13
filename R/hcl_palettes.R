#' HCL Color Palettes
#' 
#' Qualitative, sequential (single-hue and multi-hue), and diverging
#' color palettes based on the HCL (hue-chroma-luminance) color model.
#' 
#' The HCL (hue-chroma-luminance) color model is a perceptual color model
#' obtained by using polar coordinates in CIE \code{\link{LUV}} space
#' (i.e., \code{\link{polarLUV}}), where steps of equal size correspond to
#' approximately equal perceptual changes in color. By taking polar coordinates
#' the resulting three dimensions capture the three perceptual axes very well:
#' hue is the type of color, chroma the colorfulness compared
#' to the corresponding gray, and luminance the brightness. This makes it relatively
#' easy to create balanced palettes through trajectories in this HCL space.
#' In contrast, in the more commonly-used \code{\link{HSV}} (hue-saturation-value)
#' model (a simple transformation of \code{\link{RGB}}), the three axes are
#' confounded so that luminance changes along with the hue leading to very
#' unbalanced palettes (see \code{\link{rainbow_hcl}} for further illustrations).
#'
#' Three types of palettes are derived based on the HCL model: \itemize{
#'   \item Qualitative: Designed for coding categorical information, i.e.,
#'     where no particular ordering of categories is available and every color
#'     should receive the same perceptual weight.
#'   \item Sequential: Designed for coding ordered/numeric information, i.e.,
#'     where colors go from high to low (or vice versa).
#'   \item Diverging: Designed for coding numeric information around a central
#'     neutral value, i.e., where colors diverge from neutral to two extremes.
#' }
#' The corresponding functions are \code{qualitative_hcl}, \code{sequential_hcl},
#' and \code{diverging_hcl}. Their construction principles are explained in more detail below.
#' At the core is the luminance axis (i.e., light-dark contrasts):
#' These are easily decoded by humans and matched to high-low differences in the underlying
#' data. Therefore, \code{sequential_hcl} palettes are always based on a \emph{monotonic}
#' luminance sequence whereas the colors in a \code{qualitative_hcl} palette all have the
#' \emph{same} luminance. Finally, \code{diverging_hcl} palettes use the same monotonic
#' luminance sequence in both \dQuote{arms} of the palette, i.e., correspond to
#' two balanced sequential palettes diverging from the same neutral value.
#' The other two axes, hue and chroma, are used to enhance the luminance information and/or
#' to further discriminate the color.
#'
#' All three palette functions specify trajectories in HCL space and hence need either
#' single values or intervals of the coordinates \code{h}, \code{c}, \code{l}. Their
#' interfaces are always designed such that \code{h}, \code{c}, \code{l} can take vector
#' arguments (as needed) but alternatively or additionally \code{h1}/\code{h2},
#' \code{c1}/\code{c2}/\code{cmax}, and \code{l1}/\code{l2} can be specified. If so,
#' the latter coordinates overwrite the former.
#'
#' \code{qualitative_hcl} distinguishes the underlying categories by a sequence of
#' hues while keeping both chroma and luminance constant to give each color in the
#' resulting palette the same perceptual weight. Thus, \code{h} should be a pair of
#' hues (or equivalently \code{h1} and \code{h2} can be used) with the starting and
#' ending hue of the palette. Then, an equidistant sequence between these hues is
#' employed, by default spanning the full color wheel (i.e, the full 360 degrees).
#' Chroma \code{c} (or equivalently \code{c1}) and luminance \code{l} (or equivalently
#' \code{l1}) are constants.
#' 
#' \code{sequential_hcl} codes the underlying numeric values by a monotonic sequence
#' of increasing (or decreasing) luminance. Thus, the \code{l} argument should provide
#' a vector of length 2 with starting and ending luminance (equivalently, \code{l1} and
#' \code{l2} can be used). Without chroma (i.e., \code{c = 0}), this simply corresponds
#' to a grayscale palette like \code{\link[grDevices]{gray.colors}}. For adding chroma, a simple
#' strategy would be to pick a single hue (via \code{h} or \code{h1}) and then decrease
#' chroma from some value (\code{c} or \code{c1}) to zero (i.e., gray) along with
#' increasing luminance. For bringing out the extremes (a dark high-chroma color vs.
#' a light gray) this is already very effective. For distinguishing also colors in the
#' middle two strategies can be employed: (a) Hue can be varied as well by specifying an
#' interval of hues in \code{h} (or beginning hue \code{h1} and ending hue \code{h2}).
#' (b) Instead of a decreasing chroma a triangular chroma trajectory can be employed
#' from \code{c1} over \code{cmax} to \code{c2} (or equivalently a vector \code{c} of
#' length 3). This yields high-chroma colors in the middle of the palette that are
#' more easily distinguished from the dark and light extremes. Finally, instead of
#' employing linear trajectories, power transformations are supported in chroma and
#' luminance via a vector \code{power} (or separate \code{p1} and \code{p2}). If
#' \code{power[2]} (or \code{p2}) for the luminance trajectory is missing, it defaults
#' to \code{power[1]}/\code{p1} from the chroma trajectory.
#'
#' \code{diverging_hcl} codes the underlying numeric values by a triangular luminance
#' sequence with different hues in the left and in the right arm of the palette. Thus,
#' it can be seen as a combination of two sequential palettes with some restrictions:
#' (a) a single hue is used for each arm of the palette, (b) chroma and luminance trajectory
#' are balanced between the two arms, (c) the neutral central value has zero chroma.
#' To specify such a palette a vector of two hues \code{h} (or equivalently \code{h1}
#' and \code{h2}), either a single chroma value \code{c} (or \code{c1}) or a vector
#' of two chroma values \code{c} (or \code{c1} and \code{cmax}), a vector of two
#' luminances \code{l} (or \code{l1} and \code{l2}), and power parameter(s) \code{power}
#' (or \code{p1} and \code{p2}) are used. For more flexible diverging palettes without
#' the restrictrictions above (and consequently more parameters)
#' \code{\link{divergingx_hcl}} is available. For backward compatibility, \code{diverge_hcl}
#' is a copy of \code{diverging_hcl}.
#'
#' To facilitate using HCL-based palettes a wide range of example palettes are
#' provided in the package and can be specified by a name instead of a set of
#' parameters/coordinates. The examples have been taken from the literature and many
#' approximate color palettes from other software packages such as ColorBrewer.org
#' (\pkg{RColorBrewer}), CARTO colors (\pkg{rcartocolor}), or \pkg{scico}. The function
#' \code{hcl_palettes} can be used to query the available pre-specified palettes. It
#' comes with a \code{print} method (listing names and types), a \code{summary} method
#' (additionally listing the underlying parameters/coordinates), and a \code{plot}
#' method that creates a \code{\link{swatchplot}} with suitable labels.
#' 
#' @param n the number of colors (\eqn{\ge 1}{>= 1}) to be in the palette.
#' @param h,h1,h2 hue value in the HCL color description, has to be in [0, 360].
#' @param c,c.,c1,c2 chroma value in the HCL color description.
#' @param l,l1,l2 luminance value in the HCL color description.
#' @param power,p1,p2 control parameter determining how chroma and luminance should
#' be increased (1 = linear, 2 = quadratic, etc.).
#' @param cmax Maximum chroma value in the HCL color description.
#' @param gamma Deprecated.
#' @param fixup logical. Should the color be corrected to a valid RGB value?
#' @param off numeric. Vector of length 2 indicating horizontal and vertical
#' offsets between the color rectangles displayed.
#' @param border character. Color of rectangle borders.
#' @param alpha numeric vector of values in the range \code{[0, 1]} for alpha
#' transparency channel (0 means transparent and 1 means opaque).
#' @param palette character. Name of HCL color palette.
#' @param rev logical. Should the color palette vector be returned in reverse order?
#' @param register character. If set to a non-empty character string, the corresponding
#' palette is registered with that name for subsequent use (within the same session).
#' @param \dots Other arguments passed to \code{\link{hex}}.
#' @param type character indicating type of HCL palette.
#' @param plot logical. Should the selected HCL color palettes be visualized?
#' @param x,object A \code{hcl_palettes} object.
#'
#' @seealso \code{\link{divergingx_hcl}}
#' @references Zeileis A, Hornik K, Murrell P (2009).  Escaping RGBland:
#' Selecting Colors for Statistical Graphics.  \emph{Computational Statistics &
#' Data Analysis}, \bold{53}, 3259--3270.
#' \doi{10.1016/j.csda.2008.11.033}
#' Preprint available from
#' \url{https://eeecon.uibk.ac.at/~zeileis/papers/Zeileis+Hornik+Murrell-2009.pdf}.
#' 
#' Stauffer R, Mayr GJ, Dabernig M, Zeileis A (2015).  Somewhere Over the
#' Rainbow: How to Make Effective Use of Colors in Meteorological
#' Visualizations.  \emph{Bulletin of the American Meteorological Society},
#' \bold{96}(2), 203--216.
#' \doi{10.1175/BAMS-D-13-00155.1}
#' @keywords color
#' @examples
#' ## overview of all _named_ HCL palettes
#' hcl_palettes()
#'
#' ## visualize
#' hcl_palettes("qualitative", plot = TRUE)
#' hcl_palettes("sequential (single-hue)", n = 7, plot = TRUE)
#' hcl_palettes("sequential (multi-hue)", n = 7, plot = TRUE)
#' hcl_palettes("diverging", n = 7, plot = TRUE)
#'
#' ## inspect a specific palette
#' ## (upper-case, spaces, etc. are ignored for matching)
#' hcl_palettes(palette = "Dark 2")
#' hcl_palettes(palette = "dark2")
#' 
#' ## set up actual colors
#' qualitative_hcl(4, h = c(0, 288), c = 50, l = 60) ## by hand
#' qualitative_hcl(4, palette = "dark2")             ## by name
#' qualitative_hcl(4, palette = "dark2", c = 80)     ## by name plus modification
#'
#' ## how HCL palettes are constructed:
#' ## by varying the perceptual properties via hue/chroma/luminance
#' swatchplot(
#'   "Hue"       = sequential_hcl(5, h = c(0, 300), c = c(60, 60), l = 65),
#'   "Chroma"    = sequential_hcl(5, h = 0, c = c(100, 0), l = 65, rev = TRUE, power = 1),
#'   "Luminance" = sequential_hcl(5, h = 260, c = c(25, 25), l = c(25, 90), rev = TRUE, power = 1),
#'   off = 0
#' )
#' 
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
#' ## to register a particular adapted palette for re-use in the same session
#' ## with a new name the register=... argument can be used once, e.g.,
#' diverging_hcl(7, palette = "Tropic", h2 = 0, register = "mytropic")
#' 
#' ## subsequently palete="mytropic" is available in diverging_hcl() and the diverging
#' ## ggplot2 scales such as scale_color_continuous_diverging() etc.
#' demoplot(diverging_hcl(11, "mytropic"), type = "map")
#' 
#' ## to register this palette in all R sessions you could place the following
#' ## code in a startup script (e.g., .Rprofile):
#' ## colorspace::diverging_hcl(7, palette = "Tropic", h2 = 0, register = "mytropic")
#' 
#' @export
hcl_palettes <- function(type = NULL, palette = NULL, plot = FALSE, n = 5L, ...)
{
  ## subset by type and name (by flexible matching)
  fx <- function(n) tolower(gsub("[-, _, \\,, (, ), \\ , \\.]", "", n))
  pals <- .colorspace_get_info("hcl_pals")
  if(!is.null(type)) {
    tytab <- c("sequential", fx(levels(pals$type)))
    type <- lapply(type, function(ty) {
      ty <- startsWith(tytab, fx(ty))
      if(all(!ty)) stop("Palette 'type' should be one of: ", paste(levels(pals$type), collapse = ", "))
      ty <- tytab[which(ty)[1L]]
      if(ty == "sequential") ty <- c("sequentialsinglehue", "sequentialmultihue")
      return(ty)
    })
    type <- unlist(type)
    type <- levels(pals$type)[tytab[-1L] %in% type]
    pals <- pals[as.character(pals$type) %in% type, , drop = FALSE]
  } else {
    pals <- pals
  }
  if(!is.null(palette)) {
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


#' @rdname hcl_palettes
#' @export
print.hcl_palettes <- function(x, ...) {
  if(nrow(x) > 1L) {
    type <- unique(as.character(x$type))
    cat("HCL palettes\n")
    for(ty in type) {
      cat("\nType: ", ty, "\n")
      cat("Names: ")
      writeLines(strwrap(paste(rownames(x)[as.character(x$type) %in% ty], collapse = ", "), exdent = 7), ...)
    }
  } else {
    cat("HCL palette")
    cat("\nName:", rownames(x))
    cat("\nType:", as.character(x$type))
    cat("\nParameter ranges:\n")
    print.data.frame(x[ , 2L:11L, drop = FALSE], row.names = FALSE, ...)
  }
  invisible(x)
}

#' @rdname hcl_palettes
#' @export
summary.hcl_palettes <- function(object, ...) {
  type <- unique(as.character(object$type))
  cat("HCL palettes\n")
  for(ty in type) {
    cat("\nType:", ty, "\n")
    cat("Parameter ranges:\n")
    print.data.frame(object[as.character(object$type) %in% ty, -1L, drop = FALSE], ...)
  }
  invisible(object)
}

#' @rdname hcl_palettes
#' @method plot hcl_palettes
#' @export
plot.hcl_palettes <- function(x, n = 5L, fixup = TRUE, off = NULL, border = NULL, ...)
{
  typ <- c("Qualitative", "Sequential (single-hue)", "Sequential (multi-hue)", "Diverging", "Diverging (flexible)")
  x$type <- as.character(x$type)
  xx <- as.matrix(x[, -1L])

  qcol <- sapply(which(x$type == typ[1L]), function(i) {
    qualitative_hcl(n = n, h1 = xx[i, "h1"], h2 = xx[i, "h2"], c1 = xx[i, "c1"], l1 = xx[i, "l1"], fixup = fixup)
  })
  qcol <- if(length(qcol) < 1L) NULL else matrix(t(qcol), ncol = n,
    dimnames = list(rownames(x)[x$type == typ[1L]], paste("Color", 1L:n)))

  scol <- sapply(which(x$type == typ[2L]), function(i) {
    sequential_hcl(n = n, h1 = xx[i, "h1"], c1 = xx[i, "c1"], c2 = xx[i, "c2"], l1 = xx[i, "l1"], l2 = xx[i, "l2"],
      p1 = xx[i, "p1"], p2 = xx[i, "p2"], cmax = xx[i, "cmax"], fixup = fixup)
  })
  scol <- if(length(scol) < 1L) NULL else matrix(t(scol), ncol = n,
    dimnames = list(rownames(x)[x$type == typ[2L]], paste("Color", 1L:n)))

  mcol <- sapply(which(x$type == typ[3L]), function(i) {
    sequential_hcl(n = n, h1 = xx[i, "h1"], h2 = xx[i, "h2"], c1 = xx[i, "c1"], c2 = xx[i, "c2"], l1 = xx[i, "l1"], l2 = xx[i, "l2"],
      p1 = xx[i, "p1"], p2 = xx[i, "p2"], cmax = xx[i, "cmax"], fixup = fixup)
  })
  mcol <- if(length(mcol) < 1L) NULL else matrix(t(mcol), ncol = n,
    dimnames = list(rownames(x)[x$type == typ[3L]], paste("Color", 1L:n)))

  dcol <- sapply(which(x$type == typ[4L]), function(i) {
    diverging_hcl(n = n, h1 = xx[i, "h1"], h2 = xx[i, "h2"], c1 = xx[i, "c1"], l1 = xx[i, "l1"], l2 = xx[i, "l2"],
      p1 = xx[i, "p1"], p2 = xx[i, "p2"], cmax = xx[i, "cmax"], fixup = fixup)
  })
  dcol <- if(length(dcol) < 1L) NULL else matrix(t(dcol), ncol = n,
    dimnames = list(rownames(x)[x$type == typ[4L]], paste("Color", 1L:n)))
  
  xcol <- sapply(which(x$type == typ[5L]), function(i) {
    divergingx_hcl(n = n, h1 = xx[i, "h1"], h2 = xx[i, "h2"], h3 = xx[i, "h3"],
      c1 = xx[i, "c1"], c2 = xx[i, "c2"], c3 = xx[i, "c3"],
      l1 = xx[i, "l1"], l2 = xx[i, "l2"], l3 = xx[i, "l3"],
      p1 = xx[i, "p1"], p2 = xx[i, "p2"], p3 = xx[i, "p3"], p4 = xx[i, "p4"],
      cmax1 = xx[i, "cmax1"], cmax2 = xx[i, "cmax2"],
      fixup = fixup)
  })
  xcol <- if(length(xcol) < 1L) NULL else matrix(t(xcol), ncol = n,
    dimnames = list(rownames(x)[x$type == typ[5L]], paste("Color", 1L:n)))
  
  ## collect colors
  col <- list(qcol, scol, mcol, dcol, xcol)
  names(col) <- typ
  col <- col[!sapply(col, is.null)]

  swatchplot(col, off = off, border = border, ...)
}


#' @rdname hcl_palettes
#' @export
qualitative_hcl <- function(n, h = c(0, 360 * (n - 1)/n), c = 80, l = 60,
  fixup = TRUE, alpha = 1, palette = NULL, rev = FALSE, register = "", ..., h1, h2, c1, l1)
{
    ## edge cases
    if(n < 1L) return(character(0L))
    
    ## process HCL coordinates: (1) palette, (2) h/c/l, (3) h1/h2/...
    ## (1) palette
    if(is.character(h)) palette <- h
    pals <- if(!is.null(palette)) {
        as.matrix(hcl_palettes(type = "Qualitative", palette = palette)[, 2L:11L])[1L, ]
    } else {
        structure(c(if(length(h) < 2L) c(h, NA) else rep_len(h, 2L), c[1L], NA, l[1L], NA, NA, NA, NA, 1), .Names = vars.pal[1L:10L])
    }
    ## (2) h/c/l
    if(!missing(h) && !is.character(h)) {
        h <- if(length(h) < 2L) c(h, NA) else rep_len(h, 2L)
        pals["h1"] <- h[1L]
        pals["h2"] <- h[2L]
    }
    if(!missing(c)) pals["c1"] <- c
    if(!missing(l))  pals["l1"] <- l
    if(!missing(fixup)) pals["fixup"] <- as.logical(fixup)
    ## (3) h1/h2/...
    if(!missing(h1)) pals["h1"] <- h1
    if(!missing(h2)) pals["h2"] <- h2
    if(!missing(c1)) pals["c1"] <- c1
    if(!missing(l1)) pals["l1"] <- l1
    
    ## register custom palette?
    if(is.character(register) && nchar(register) > 0L) {
      add_hcl_pals(palette = register, type = "Qualitative", parameters = pals)
      register <- TRUE
    } else {
      register <- FALSE
    }

    ## explicitly expand h2 if still NA    
    if(is.na(pals["h2"])) pals["h2"] <- pals["h1"] + 360 * (n - 1)/n

    ## HCL trajectory
    rval <- hex(polarLUV(
        L = pals["l1"],
        C = pals["c1"],
        H = seq(pals["h1"], pals["h2"], length = n)),
        fixup = as.logical(pals["fixup"]), ...)

    ## alpha transparency
    if(!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
        alpha <- format(as.hexmode(round(alpha * 255 + 0.0001)), width = 2L, upper.case = TRUE)
        rval <- ifelse(is.na(rval), NA, paste(rval, alpha, sep = ""))
    }

    ## return value
    if(rev) rval <- rev(rval)
    if(register) invisible(rval) else return(rval)
}

#' @rdname hcl_palettes
#' @export
sequential_hcl <- function(n, h = 260, c = 80, l = c(30, 90), power = 1.5,
  gamma = NULL, fixup = TRUE, alpha = 1, palette = NULL, rev = FALSE, register = "", ...,
  h1, h2, c1, c2, l1, l2, p1, p2, cmax, c.)
{
    ## edge cases
    if (!is.null(gamma)) warning("'gamma' is deprecated and has no effect")
    if(n < 1L) return(character(0L))
    if(!missing(c.)) c <- c.
    if(length(c) == 3L) c <- c[c(1L, 3L, 2L)]

    ## process HCL coordinates: (1) palette, (2) h/c/l, (3) h1/h2/...
    ## (1) palette
    if(is.character(h)) palette <- h
    pals <- if(!is.null(palette)) {
        as.matrix(hcl_palettes(type = "Sequential", palette = palette)[, 2L:11L])[1L, ]
    } else {
        structure(c(
            if(length(h) < 2L) c(h, NA) else rep_len(h, 2L),
            if(length(c) < 2L) c(c, 0) else rep_len(c, 2L),
            rep_len(l, 2L),
            if(length(power) < 2L) c(power, NA) else rep_len(power, 2L),
            if(length(c) < 3L) NA else c[3L],
            1), .Names = vars.pal)
    }
    ## (2) h/c/l
    if(!missing(h) && !is.character(h)) {
        h <- if(length(h) < 2L) c(h, NA) else rep_len(h, 2L)
        pals["h1"] <- h[1L]
        pals["h2"] <- h[2L]
    }
    if(!missing(c) || !missing(c.)) {
        if(length(c) < 2L) c <- c(c, 0)
        pals["c1"] <- c[1L]
        pals["c2"] <- c[2L]
        if(length(c) == 3L) pals["cmax"] <- c[3L]
    }
    if(!missing(l)) {
        l <- rep_len(l, 2L)
        pals["l1"] <- l[1L]
        pals["l2"] <- l[2L]
    }
    if(!missing(power)) {
        power <- if(length(power) < 2L) c(power, NA) else rep_len(power, 2L)
        pals["p1"] <- power[1L]
        pals["p2"] <- power[2L]
    }
    if(!missing(fixup)) pals["fixup"] <- as.logical(fixup)
    ## (3) h1/h2/...
    if(!missing(h1)) pals["h1"] <- h1
    if(!missing(h2)) pals["h2"] <- h2
    if(!missing(c1)) pals["c1"] <- c1
    if(!missing(c2)) pals["c2"] <- c2
    if(!missing(l1)) pals["l1"] <- l1
    if(!missing(l2)) pals["l2"] <- l2
    if(!missing(p1)) pals["p1"] <- p1
    if(!missing(p2)) pals["p2"] <- p2
    if(!missing(cmax)) pals["cmax"] <- cmax
    if(!is.na(pals["h2"]) && pals["h1"] == pals["h2"]) pals["h2"] <- NA

    ## register custom palette?
    if(is.character(register) && nchar(register) > 0L) {
      add_hcl_pals(palette = register,
        type = if(is.na(pals["h2"])) "Sequential (single-hue)" else "Sequential (multi-hue)",
	parameters = pals)
      register <- TRUE
    } else {
      register <- FALSE
    }

    ## expand parameters that are potentially NA
    if(is.na(pals["h2"])) pals["h2"] <- pals["h1"]
    if(is.na(pals["c2"])) pals["c2"] <- 0
    if(is.na(pals["p2"])) pals["p2"] <- pals["p1"]

    ## HCL trajectory
    cmaxat <- 1/(1 + abs(pals["cmax"] - pals["c1"]) / abs(pals["cmax"] - pals["c2"]))
    if ( ! is.na(cmaxat) && (cmaxat <= 0 | cmaxat >= 1) ) cmaxat <- NA
    rval <- seq(1, 0, length = n)
    rval <- hex(polarLUV(
        L = pals["l2"] - (pals["l2"] - pals["l1"]) * rval^pals["p2"],
        C = if(is.na(cmaxat)) {
              pals["c2"] - (pals["c2"] - pals["c1"]) * rval^pals["p1"]
            } else {
              ifelse(rval^pals["p1"] <= cmaxat,
                     pals["c2"] - (pals["c2"] - pals["cmax"]) * (rval^pals["p1"])/cmaxat,
                     pals["cmax"] - (pals["cmax"] - pals["c1"]) * ((rval^pals["p1"] - cmaxat)/(1 - cmaxat))
              )
            },
        H = pals["h2"] - (pals["h2"] - pals["h1"]) * rval),
        fixup = as.logical(pals["fixup"]), ...)

    ## alpha transparency
    if(!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
        alpha <- format(as.hexmode(round(alpha * 255 + 0.0001)), width = 2L, upper.case = TRUE)
        rval <- ifelse(is.na(rval), NA, paste(rval, alpha, sep = ""))
    }

    ## return value
    if(rev) rval <- rev(rval)
    if(register) invisible(rval) else return(rval)
}

#' @rdname hcl_palettes
#' @export
diverging_hcl <- function(n, h = c(260, 0), c = 80, l = c(30, 90), power = 1.5,
  gamma = NULL, fixup = TRUE, alpha = 1, palette = NULL, rev = FALSE, register = "", ...,
  h1, h2, c1, l1, l2, p1, p2, cmax)
{
    ## edge cases
    if (!is.null(gamma)) warning("'gamma' is deprecated and has no effect")
    if(n < 1L) return(character(0L))

    ## process HCL coordinates: (1) palette, (2) h/c/l, (3) h1/h2/...
    ## (1) palette
    if(is.character(h)) palette <- h
    pals <- if(!is.null(palette)) {
        as.matrix(hcl_palettes(type = "Diverging", palette = palette)[, 2L:11L])[1L, ]
    } else {
        structure(c(
            rep_len(h, 2L),
            c(c[1L], NA),
            rep_len(l, 2L),
            if(length(power) < 2L) c(power, NA) else rep_len(power, 2L),
            if(length(c) > 1L) c[2L] else NA,
            1), .Names = vars.pal)
    }
    ## (2) h/c/l
    if(!missing(h) && !is.character(h)) {
        h <- rep_len(h, 2L)
        pals["h1"] <- h[1L]
        pals["h2"] <- h[2L]
    }
    if(!missing(c)) {
      pals["c1"] <- c[1L]
      if(length(c) > 1L) pals["cmax"] <- c[2L]
    }
    if(!missing(l)) {
        l <- rep_len(l, 2L)
        pals["l1"] <- l[1L]
        pals["l2"] <- l[2L]
    }
    if(!missing(power)) {
        power <- if(length(power) < 2L) c(power, NA) else rep_len(power, 2L)
        pals["p1"] <- power[1L]
        pals["p2"] <- power[2L]
    }
    if(!missing(fixup)) pals["fixup"] <- as.logical(fixup)
    ## (3) h1/h2/...
    if(!missing(h1)) pals["h1"] <- h1
    if(!missing(h2)) pals["h2"] <- h2
    if(!missing(c1)) pals["c1"] <- c1
    if(!missing(l1)) pals["l1"] <- l1
    if(!missing(l2)) pals["l2"] <- l2
    if(!missing(p1)) pals["p1"] <- p1
    if(!missing(p2)) pals["p2"] <- p2
    if(!missing(cmax)) pals["cmax"] <- cmax
    pals["c2"] <- NA

    ## register custom palette?
    if(is.character(register) && nchar(register) > 0L) {
      add_hcl_pals(palette = register, type = "Diverging", parameters = pals)
      register <- TRUE
    } else {
      register <- FALSE
    }

    ## expand parameters that are potentially NA
    if(is.na(pals["p2"])) pals["p2"] <- pals["p1"]

    ## HCL trajectory
    cmaxat <- 1/(1 + abs(pals["cmax"] - pals["c1"]) / pals["cmax"])
    if ( ! is.na(cmaxat) && (cmaxat <= 0 | cmaxat >= 1) ) cmaxat <- NA
    rval <- seq(1, -1, length = n)
    rval <- hex(polarLUV(
        L = pals["l2"] - (pals["l2"] - pals["l1"]) * abs(rval)^pals["p2"],
        C = if(is.na(cmaxat)) {
              pals["c1"] * abs(rval)^pals["p1"]
            } else {
              ifelse(abs(rval)^pals["p1"] <= cmaxat,
                pals["cmax"] * (abs(rval)^pals["p1"])/cmaxat,
                    pals["cmax"] - (pals["cmax"] - pals["c1"]) * ((abs(rval)^pals["p1"] - cmaxat)/(1 - cmaxat))
              )
            },
        H = ifelse(rval > 0, pals["h1"], pals["h2"])),
        fixup = as.logical(pals["fixup"]), ...)

    ## alpha transparency
    if(!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
        alpha <- format(as.hexmode(round(alpha * 255 + 0.0001)), width = 2L, upper.case = TRUE)
        rval <- ifelse(is.na(rval), NA, paste(rval, alpha, sep = ""))
    }

    ## return value
    if(rev) rval <- rev(rval)
    if(register) invisible(rval) else return(rval)
}

#' @rdname hcl_palettes
#' @usage NULL
#' @export
diverge_hcl <- diverging_hcl


# -------------------------------------------------------------------
# Palette specifications
# -------------------------------------------------------------------

vars.pal <- c("h1", "h2", "c1", "c2", "l1", "l2", "p1", "p2", "cmax", "fixup")
                                                                              # Inspired by:
qual.pals <- list()
qual.pals[["Pastel 1"]]    <- c(  0,   NA,  35, NA, 85, NA,  NA,  NA,  NA, 1) # ColorBrewer.org: Pastel1
qual.pals[["Dark 2"]]      <- c(  0,   NA,  50, NA, 60, NA,  NA,  NA,  NA, 1) # ColorBrewer.org: Dark2
qual.pals[["Dark 3"]]      <- c(  0,   NA,  80, NA, 60, NA,  NA,  NA,  NA, 1) # JCF/Z: ~Dark2 with more chroma
qual.pals[["Set 2"]]       <- c(  0,   NA,  60, NA, 70, NA,  NA,  NA,  NA, 1) # ColorBrewer.org: Set2
qual.pals[["Set 3"]]       <- c( 10,   NA,  50, NA, 80, NA,  NA,  NA,  NA, 1) # ColorBrewer.org: Set3
qual.pals[["Warm"]]        <- c( 90,  -30,  50, NA, 70, NA,  NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Warm (based on Ihaka-03)
qual.pals[["Cold"]]        <- c(270,  150,  50, NA, 70, NA,  NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Cold (based on Ihaka-03)
qual.pals[["Harmonic"]]    <- c( 60,  240,  50, NA, 70, NA,  NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Harmonic (based on Ihaka-03)
qual.pals[["Dynamic"]]     <- c( 30,   NA,  50, NA, 70, NA,  NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Dynamic (based on Ihaka-03)

seqs.pals <- list()
seqs.pals[["Grays"]]       <- c(  0,   NA,   0, NA, 10, 98, 1.3,  NA,  NA, 1) # ColorBrewer.org: Greys
seqs.pals[["Light Grays"]] <- c(  0,   NA,   0, NA, 30, 90, 1.5,  NA,  NA, 1) # Z+KH+PM-09, Fig.5: Light Grays
seqs.pals[["Blues 2"]]     <- c(260,   NA,  80, NA, 30, 90, 1.5,  NA,  NA, 1) # Z+KH+PM-09, Fig.5: Blues
seqs.pals[["Blues 3"]]     <- c(245,   NA,  50, NA, 20, 98, 0.8, 1.4,  75, 1) # ColorBrewer.org: Blues
seqs.pals[["Purples 2"]]   <- c(270,   NA,  70, NA, 25, 95, 1.2,  NA,  NA, 1) # ColorBrewer.org: Purples
seqs.pals[["Purples 3"]]   <- c(270,   NA,  50, NA, 20, 98, 0.9, 1.4,  75, 1) # ColorBrewer.org: Purples
seqs.pals[["Reds 2"]]      <- c( 10,   NA,  85, NA, 25, 95, 1.3,  NA,  NA, 1) # ColorBrewer.org: Reds
seqs.pals[["Reds 3"]]      <- c( 10,   NA,  65, NA, 20, 97, 1.1, 1.3, 150, 1) # ColorBrewer.org: Reds
seqs.pals[["Greens 2"]]    <- c(135,   NA,  45, NA, 35, 95, 1.3,  NA,  NA, 1) # ColorBrewer.org: Greens
seqs.pals[["Greens 3"]]    <- c(135,   NA,  35, NA, 25, 98, 1.0, 1.5,  70, 1) # ColorBrewer.org: Greens
seqs.pals[["Oslo"]]        <- c(250,   NA,   0,  0, 99,  1, 1.0,  NA,  70, 1) # scico: oslo

seqm.pals <- list()
seqm.pals[["Purple-Blue"]] <- c(300,  200,  60,  0, 25, 95, 0.7, 1.3,  NA, 1) # ColorBrewer.org: BuPu
seqm.pals[["Red-Purple"]]  <- c( 10,  -80,  80,  5, 25, 95, 0.7, 1.3,  NA, 1) # ColorBrewer.org: PuRd
seqm.pals[["Red-Blue"]]    <- c(  0, -100,  80, 40, 40, 75, 1.0, 1.0,  NA, 1) # Z+KH+PM-09, Fig.5: Red-Blue
seqm.pals[["Purple-Orange"]]<-c(-83,   20,  65, 18, 32, 90, 0.5, 1.0,  NA, 1) # CARTO: PurpOr
seqm.pals[["Blue-Yellow"]] <- c(265,   80,  60, 10, 25, 95, 0.7, 2.0,  NA, 1) # ColorBrewer.org: YlGnBu
seqm.pals[["Green-Yellow"]]<- c(140,   80,  50, 10, 40, 97, 0.7, 1.8,  NA, 1) # ColorBrewer.org: YlGn
seqm.pals[["Red-Yellow"]]  <- c( 10,   85,  80, 10, 25, 95, 0.4, 1.3,  NA, 1) # ColorBrewer.org: YlOrRd
seqm.pals[["Heat"]]        <- c(  0,   90,  80, 30, 30, 90, 0.2, 2.0,  NA, 1) # JCF/Z: alternative to heat_hcl
seqm.pals[["Heat 2"]]      <- c(  0,   90, 100, 30, 50, 90, 0.2, 1.0,  NA, 1) # Z+KH+PM-09, Fig.5: heat_hcl
seqm.pals[["Terrain"]]     <- c(130,    0,  80,  0, 60, 95, 0.1, 1.0,  NA, 1) # Z+KH+PM-09, Fig.5: terrain_hcl
seqm.pals[["Terrain 2"]]   <- c(130,   30,  65,  0, 45, 90, 0.5, 1.5,  NA, 1) # JCF/Z: alternative to terrain_hcl

seqm.pals[["Viridis"]]     <- c(300,   75,  40, 95, 15, 90, 1.0, 1.1,  NA, 1) # viridis::viridis
seqm.pals[["Plasma"]]      <-c(-100,  100,  60,100, 15, 95, 2.0, 0.9,  NA, 1) # viridis::plasma
seqm.pals[["Inferno"]]     <-c(-100,   85,   0, 65,  1, 98, 1.1, 0.9, 120, 1) # viridis::inferno

seqm.pals[["Dark Mint"]]   <- c(240,  130,  30, 33, 25, 95, 1.0,  NA,  NA, 1) # CARTO: Dark Mint
seqm.pals[["Mint"]]        <- c(205,  140,  40, 12, 34, 94, 0.5, 1.0,  NA, 1) # CARTO: Mint
seqm.pals[["BluGrn"]]      <- c(215,  120,  25, 30, 31, 88, 0.7, 1.1,  45, 1) # CARTO: BluGrn
seqm.pals[["Teal"]]        <- c(240,  180,  35, 15, 35, 92, 0.6, 1.1,  40, 1) # CARTO: Teal
seqm.pals[["TealGrn"]]     <- c(220,  125,  44, 50, 49, 90, 0.8, 1.2,  60, 1) # CARTO: TealGrn
seqm.pals[["Emrld"]]       <- c(224,  105,  23, 55, 25, 92, 1.5, 1.0,  NA, 1) # CARTO: Emrld
seqm.pals[["BluYl"]]       <- c(250,   90,  40, 55, 33, 98, 0.5, 1.0,  NA, 1) # CARTO: BluYl
seqm.pals[["ag_GrnYl"]]    <- c(225,   87,  27, 86, 34, 92, 0.9,  NA,  NA, 1) # CARTO: ag_GrnYl
seqm.pals[["Peach"]]       <- c( 15,   50, 128, 30, 55, 90, 1.1,  NA,  NA, 1) # CARTO: Peach
seqm.pals[["PinkYl"]]      <- c( -4,   80, 100, 47, 55, 96, 1.0,  NA,  NA, 1) # CARTO: PinkYl
seqm.pals[["Burg"]]        <- c(-10,   10,  40, 40, 25, 85, 1.2, 1.0,  75, 1) # CARTO: Burg
seqm.pals[["BurgYl"]]      <- c(-10,   55,  45, 30, 30, 90, 0.7, 1.0,  80, 1) # CARTO: BurgYl
seqm.pals[["RedOr"]]       <- c( -3,   53,  75, 42, 44, 86, 0.8, 1.0,  90, 1) # CARTO: RedOr
seqm.pals[["OrYel"]]       <- c(  5,   72, 120, 49, 56, 87, 1.0,  NA, 125, 1) # CARTO: OrYel
seqm.pals[["Purp"]]        <- c(270,  300,  55, 20, 42, 92, 0.6, 1.0,  60, 1) # CARTO: Purp
seqm.pals[["PurpOr"]]      <- c(-83,   20,  55, 18, 32, 90, 0.6, 1.0,  65, 1) # CARTO: PurpOr
seqm.pals[["Sunset"]]      <- c(-80,   78,  60, 55, 40, 91, 0.8, 1.0,  75, 1) # CARTO: Sunset
seqm.pals[["Magenta"]]     <- c(312,  358,  50, 24, 27, 85, 0.6, 1.1,  65, 1) # CARTO: Magenta
seqm.pals[["SunsetDark"]]  <- c(-35,   50,  55, 60, 30, 90, 1.2, 1.0, 120, 1) # CARTO: SunsetDark
seqm.pals[["ag_Sunset"]]   <- c(-85,   70,  70, 45, 25, 85, 0.6, 1.0, 105, 1) # CARTO: ag_Sunset
seqm.pals[["BrwnYl"]]      <- c(-20,   70,  30, 20, 20, 90, 1.0, 1.1,  60, 1) # CARTO: BrwnYl

seqm.pals[["YlOrRd"]]      <- c(  5,   85,  75, 40, 25, 99, 1.6, 1.3, 180, 1) # ColorBrewer.org: YlOrRd
seqm.pals[["YlOrBr"]]      <- c( 20,   85,  50, 20, 25, 99, 1.3, 1.5, 150, 1) # ColorBrewer.org: YlOrBr
seqm.pals[["OrRd"]]        <- c(  0,   60,  90, 10, 25, 97, 1.0, 1.5, 135, 1) # ColorBrewer.org: OrRd
seqm.pals[["Oranges"]]     <- c( 20,   55,  70, 10, 30, 97, 1.2, 1.3, 150, 1) # ColorBrewer.org: Oranges
seqm.pals[["YlGn"]]        <- c(160,   85,  25, 20, 25, 99, 1.2, 1.6,  70, 1) # ColorBrewer.org: YlGn
seqm.pals[["YlGnBu"]]      <- c(270,   90,  40, 25, 15, 99, 2.0, 1.5,  90, 1) # ColorBrewer.org: YlGnBu
seqm.pals[["Reds"]]        <- c(  0,   35,  65,  5, 20, 97, 1.1, 1.3, 150, 1) # ColorBrewer.org: Reds
seqm.pals[["RdPu"]]        <- c(-70,   40,  45,  5, 15, 97, 1.0, 1.3, 100, 1) # ColorBrewer.org: RdPu
seqm.pals[["PuRd"]]        <- c( 20,  -95,  60,  5, 20, 97, 1.6, 1.1, 140, 1) # ColorBrewer.org: PuRd
seqm.pals[["Purples"]]     <- c(275,  270,  55,  5, 20, 99, 1.3, 1.3,  70, 1) # ColorBrewer.org: Purples
seqm.pals[["PuBuGn"]]      <- c(160,  320,  25,  5, 25, 98, 1.4, 1.2,  70, 1) # ColorBrewer.org: PuBuGn
seqm.pals[["PuBu"]]        <- c(240,  260,  30,  5, 25, 98, 1.5, 1.2,  70, 1) # ColorBrewer.org: PuBu
seqm.pals[["Greens"]]      <- c(135,  115,  35,  5, 25, 98, 1.0, 1.5,  70, 1) # ColorBrewer.org: Greens
seqm.pals[["BuGn"]]        <- c(125,  200,  30,  5, 25, 98, 1.4, 1.6,  65, 1) # ColorBrewer.org: BuGn
seqm.pals[["GnBu"]]        <- c(265,   95,  55, 10, 25, 97, 1.3, 1.7,  80, 1) # ColorBrewer.org: GnBu
seqm.pals[["BuPu"]]        <- c(320,  200,  40,  5, 15, 98, 1.2, 1.3,  65, 1) # ColorBrewer.org: BuPu
seqm.pals[["Blues"]]       <- c(260,  220,  45,  5, 25, 98, 1.2, 1.3,  70, 1) # ColorBrewer.org: Blues
seqm.pals[["Lajolla"]]     <- c( 90,  -20,  40,  5, 99,  5, 0.7, 0.8, 100, 1) # scico: lajolla
seqm.pals[["Turku"]]       <- c( 10,  120,  20,  0, 95,  1, 1.7, 0.8,  55, 1) # scico: turku

dive.pals <- list()
dive.pals[["Blue-Red"]]    <- c(260,    0,  80, NA, 30, 90, 1.5,  NA,  NA, 1) # Z+KH+PM-09, Fig.6: Blue-Red (high luminance contrast)
dive.pals[["Blue-Red 2"]]  <- c(260,    0, 100, NA, 50, 90, 1.0,  NA,  NA, 1) # Z+KH+PM-09, Fig.6: Blue-Red (medium luminance contrast)
dive.pals[["Blue-Red 3"]]  <- c(255,   12,  50, NA, 20, 97, 1.0, 1.3,  80, 1) # ColorBrewer.org: RdBu
dive.pals[["Red-Green"]]   <- c(340,  128,  60, NA, 30, 97, 0.8, 1.5,  80, 1) # ColorBrewer.org: PiYG
dive.pals[["Purple-Green"]]<- c(300,  128,  30, NA, 20, 95, 1.0, 1.4,  65, 1) # ColorBrewer.org: PRGn
dive.pals[["Purple-Brown"]]<- c(270,   40,  30, NA, 20, 98, 0.8, 1.2,  70, 1) # ColorBrewer.org: PuOr
dive.pals[["Green-Brown"]] <- c(180,   55,  40, NA, 25, 97, 0.8, 1.4,  65, 1) # ColorBrewer.org: BrBG
dive.pals[["Blue-Yellow 2"]]<-c(265,   80,  80, NA, 40, 95, 1.2,  NA,  NA, 1) # Z+COW
dive.pals[["Blue-Yellow 3"]]<-c(265,   80,  80, NA, 70, 95, 0.5, 2.0,  NA, 1) # Z+COW
dive.pals[["Green-Orange"]]<- c(130,   43, 100, NA, 70, 90, 1.0,  NA,  NA, 1) # Z+KH+PM-09, Fig.6: Green-Orange (low luminance contrast)
dive.pals[["Cyan-Magenta"]]<- c(180,  330,  59, NA, 75, 95, 1.5,  NA,  NA, 1) # Z+KH+PM-09, Fig.6: Blue-Red (low luminance contrast)
dive.pals[["Tropic"]]      <- c(195,  325,  70, NA, 55, 95, 1.0,  NA,  NA, 1) # CARTO: Tropic
dive.pals[["Broc"]]        <- c(240,   85,  30, NA, 15, 98, 0.9,  NA,  45, 1) # scico: broc
dive.pals[["Cork"]]        <- c(245,  125,  30, NA, 15, 95, 0.9, 1.1,  55, 1) # scico: cork
dive.pals[["Vik"]]         <- c(240,   55,  45, NA, 15, 95, 0.8, 1.1,  65, 1) # scico: vik
dive.pals[["Berlin"]]      <- c(240,   15,  60, NA, 75,  5, 1.2, 1.5,  80, 1) # scico: berlin
dive.pals[["Lisbon"]]      <- c(240,   85,  30, NA, 98,  8, 1.0,  NA,  45, 1) # scico: lisbon
dive.pals[["Tofino"]]      <- c(260,  120,  45, NA, 90,  5, 0.8, 1.0,  55, 1) # scico: tofino



base.pals <- list()
base.pals[["rainbow"]]        <- c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, 1)    # Default RGB rainbow
base.pals[["heat.colors"]]    <- c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, 1)    # Default heatmap
base.pals[["topo.colors"]]    <- c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, 1)    # Default topo colors
base.pals[["terrain.colors"]] <- c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, 1)    # Default terrain colors
base.pals[["cm.colors"]]      <- c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, 1)    # Default cyan magenta colors
base.pals[["bpy"]]            <- c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, 1)    # Analogous to sp::bpy.colors

## collect all hcl palettes
make_hcl_pals <- function() {
  ## collect all palettes by group
  qpals <- as.data.frame(do.call("rbind", qual.pals))
  rownames(qpals) <- names(qual.pals)
  qpals$type <- "Qualitative"

  spals <- as.data.frame(do.call("rbind", seqs.pals))
  rownames(spals) <- names(seqs.pals)
  spals$type <- "Sequential (single-hue)"

  mpals <- as.data.frame(do.call("rbind", seqm.pals))
  rownames(mpals) <- names(seqm.pals)
  mpals$type <- "Sequential (multi-hue)"

  dpals <- as.data.frame(do.call("rbind", dive.pals))
  rownames(dpals) <- names(dive.pals)
  dpals$type <- "Diverging"
  
  ## combine and rearrange
  pals <- rbind(qpals, spals, mpals, dpals)
  names(pals) <- c(vars.pal, "type")
  pals$type <- factor(pals$type, levels = c("Qualitative",
    "Sequential (single-hue)", "Sequential (multi-hue)", "Diverging"))
  pals$fixup <- as.logical(pals$fixup)
  pals <- pals[, c("type", names(pals)[!names(pals) %in% "type"])]
  return(pals)
}

.colorspace_set_info(
  hcl_pals = make_hcl_pals()
)

add_hcl_pals <- function(palette, type, parameters) {
  pals <- .colorspace_get_info("hcl_pals")
  p <- data.frame(type = factor(type, levels = levels(pals$type)))
  p <- cbind(p, as.data.frame(as.list(parameters)))
  p$fixup <- as.logical(p$fixup)
  pals[palette, ] <- p
  pals <- pals[order(pals$type), ]
  .colorspace_set_info(hcl_pals = pals)
}


# -------------------------------------------------------------------
# Character vector specifying the example plots. For each element
# the function plot_<name> will be called.
# -------------------------------------------------------------------
example.plots <- c("Map", "Heatmap", "Scatter", "Spine", "Bar",
                   "Pie", "Perspective", "Mosaic", "Lines", "Spectrum", "HCL Plot")

# -------------------------------------------------------------------
# Helper function: returns a data.frame containing all
# palettes specified above. Used for hclwizard and tcltk interface.
# @param gui, `NULL` or logical. If ``NULL` all palettes will be
# returned. If TRUE or FALSE the palettes will be subsetted and the
# data.frame will be slightly modified to fulfill the requirements
# for the graphical user interfaces (tcltk and shiny).
# -------------------------------------------------------------------
GetPaletteConfig <- function(gui = NULL) {

   res <- NULL
   palettes <- list(
     qual = qual.pals,
     seqs = seqs.pals,
     seqm = seqm.pals,
     dive = dive.pals,
     base = base.pals
   )
   res <- lapply(names(palettes), function(type, palettes) {
              # Palette parameters to data.frame
              x <- as.data.frame(do.call(rbind, palettes[[type]]))
              names(x) <- toupper(vars.pal)
              # Append type
              cbind(data.frame("type" = rep(type, nrow(x))), x)
          }, palettes = palettes)
   # Return data.frame
   pals      <- do.call(rbind, res)
   pals$type <- as.character(pals$type)

   if ( inherits(gui, "logical") ) {
       take <- c(
           # Qualitative
           "Pastel 1", "Dark 2", "Dark 3", "Set 2", "Set 3", "Warm", "Cold", "Harmonic", "Dynamic",
           # Diverging
           "Blue-Red", "Blue-Red 2", "Blue-Yellow 2", "Blue-Yellow 3",
           "Green-Orange", "Cyan-Magenta", "Tropic",
           # Diverging advanced
           "Blue-Red 3", "Red-Green", "Purple-Green", "Purple-Brown",
           "Green-Brown", "Cork", "Berlin", "Lisbon", "Tofino",
           # Sequential single-hue
           "Grays", "Light Grays", "Blues 2", "Purples 2", "Reds 2", "Greens 2",
           # Sequential single-hue advanced
           "Blues 3", "Purples 3", "Reds 3", "Greens 3", "Oslo",
           # Sequential multiple-hues
           "Purple-Blue", "Purple-Orange", "Red-Blue", "Red-Purple",
           "Red-Yellow", "Heat", "PinkYl", "Green-Yellow", "Terrain 2",
	   "Dark Mint", "BluYl", "Blue-Yellow", "Viridis", "Plasma",
           # Sequential multiple-hues advanced
           "YlGnBu", "Greens", "BuGn", "Teal", "Peach", "Blues", "BuPu", "Purples",
           "Purp", "Burg", "Reds", "YlOrRd", "Sunset", "RdPu", "Inferno",
           "Lajolla", "Turku",
           # Base color maps (for shiny)
           "rainbow", "heat.colors", "topo.colors", "terrain.colors", "cm.colors", "bpy"
       ) # end of variable definition for "take"

       # For qualitative: set h2 if h2 is NA (else the sliders will
       # be disabled on the graphical user interfaces).
       idx <- which(pals$type == "qual" & is.na(pals$H2))
       pals$H2[idx] <- ifelse((pals$H1[idx] + 360) > 360, pals$H1[idx] - 360, pals$H1[idx] + 360)

       # Subset
       mtch <- match(take, rownames(pals))
       pals <- pals[mtch,]
       #pals <- pals[which(rownames(pals) %in% take),]

       # Extending the type names for use in GUIs
       idx <- which(with(pals, (type == "dive" &  !is.na(CMAX) |
                               (type == "seqs" & (!is.na(CMAX) | !is.na(P2))) |
                               (type == "seqm" & (!is.na(CMAX) | C1 > 100 | C2 > 100)))))
       if ( length(idx) > 0 ) pals$type[idx] <- sprintf("%s_advanced", pals$type[idx])
   }

   return(pals)
}

