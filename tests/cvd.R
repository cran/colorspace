library("colorspace")


## convert a hex color
simulate_cvd("#33ab20", tritanomaly_cvd["6"][[1]])

## convert a named color
simulate_cvd("red", deutanomaly_cvd["8"][[1]])

## convert a mixed vector (hex and named)
simulate_cvd(c("green", "#ffc0cb"), protanomaly_cvd["8"][[1]])

## white and black unchanged
## simulate_cvd(c("white", "black"), deutanomaly_cvd["2"][[1]])


## make sure that different sequences of conversions lead to equivalent output

## vector of different hex colors
col_hex <- c(rainbow(33), sequential_hcl(33, "Plasma"), sequential_hcl(33, "YlGnBu"))

## corresponding color object in sRGB and linear RGB
col_srgb <- hex2RGB(col_hex)
col_rgb <- as(col_srgb, "RGB")

## corresponding RGB coordinates in 0-255
mat_srgb <- coords(col_srgb) * 255
mat_rgb <- coords(col_rgb) * 255

## protan transformation in linear RGB (default) and gamma-corrected sRGB (linear = FALSE)
identical(protan(col_hex),                 hex(protan(col_srgb)))
identical(protan(col_hex),                 hex(protan(col_rgb)))
identical(protan(col_hex),                 hex(RGB(protan(mat_rgb)/255)))
identical(protan(col_hex, linear = FALSE), hex(protan(col_srgb, linear = FALSE)))
identical(protan(col_hex, linear = FALSE), hex(protan(col_rgb, linear = FALSE)))
identical(protan(col_hex, linear = FALSE), hex(sRGB(protan(mat_srgb)/255)))
