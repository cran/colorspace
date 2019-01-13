# rescaling functions needed for ggplot continuous color scales
# the scales dependency is automatically satisfied when ggplot2 is installed

# returns a rescaling function that scales to a target range
to_rescaler <- function(begin = 0, end = 1) {
  function(x, to = c(begin, end), from = range(x, na.rm = TRUE)) {
    scales::rescale(x, to, from)
  }
}

# returns a rescaling function that scales to a target midpoint
mid_rescaler <- function(mid) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}
