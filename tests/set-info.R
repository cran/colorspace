library("colorspace")

## mixed named and unnamed arguments
suppressWarnings(
  colorspace:::.colorspace_set_info("ignored", colorspace_unnamed_arg_test = TRUE)
)
stopifnot(isTRUE(colorspace:::.colorspace_get_info("colorspace_unnamed_arg_test")))
colorspace:::.colorspace_set_info(colorspace_unnamed_arg_test = NULL)
