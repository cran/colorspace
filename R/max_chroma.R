#' Compute Maximum Chroma for Given Hue and Luminance in HCL
#' 
#' Compute approximately the maximum chroma possible for a given hue and
#' luminance combination in the HCL color space.
#' 
#' As the possible combinations of chroma and luminance depend on hue, it is
#' not obvious which maximum chroma can be used for a given combination of hue
#' and luminance prior to calling \code{\link{polarLUV}}. To avoid having to
#' \code{fixup} the color upon conversion to RGB \code{\link{hex}} codes, the
#' \code{max_chroma} function computes (approximately) the maximum chroma
#' possible. The computations are based on interpolations of pre-computed
#' maxima in \code{max_chroma_table}, containing the maximum chroma for a given
#' hue-luminance combination (both in integers). Hence, the result may sometimes
#' still be very slightly larger than the actual maximum which can be avoided
#' by taking the \code{floor} of the approximate value.
#' 
#' @param h hue value in the HCL color description, has to be in [0, 360].
#' @param l luminance value in the HCL color description, has to be in [0, 100].
#' @param floor logical. Should the chroma value be rounded down to the next
#' lower integer?
#' @return A numeric vector with the maximum chroma coordinates.
#' @seealso \code{\link[colorspace]{polarLUV}}, \code{\link[colorspace]{hex}}
#' @keywords color
#' @examples
#' max_chroma(h = 0:36 * 10, l = 50)
#' max_chroma(h = 120, l = 0:10 * 10)
#' @export
max_chroma <- function(h, l, floor = FALSE) {
  ## align h and l
  n <- max(c(length(h), length(l)))
  h <- rep_len(h, n)
  l <- rep_len(l, n)

  ## assure h in [0, 360]
  while(any(h < 0)) h[h < 0] <- h[h < 0] + 360
  while(any(h >= 360)) h[h >= 360] <- h[h >= 360] - 360

  ## assure l in [0, 100]
  l <- pmin(100, pmax(0, l))

  ## obtain surrounding h/l coordinates
  hmin <- floor(h + 1e-8)
  hmax <- ceiling(h + 1e-8)
  lmin <- floor(l + 1e-8)
  lmax <- ceiling(l + 1e-8)

  ## average
  c <- (hmax - h) * (lmax - l) * colorspace::max_chroma_table[paste(hmin, lmin, sep = "-")] + 
       (hmax - h) * (l - lmin) * colorspace::max_chroma_table[paste(hmin, lmax, sep = "-")] + 
       (h - hmin) * (lmax - l) * colorspace::max_chroma_table[paste(hmax, lmin, sep = "-")] + 
       (h - hmin) * (l - lmin) * colorspace::max_chroma_table[paste(hmax, lmax, sep = "-")]

  ## catch border cases
  c <- as.numeric(c) # pmin(c, 100)
  c[l <= 0 | l >= 100] <- 0
  
  ## take floor to be "on the safe side"
  if(floor) c <- floor(c)
  
  return(c)
}

#' @rdname max_chroma
#' @format NULL
"max_chroma_table"
