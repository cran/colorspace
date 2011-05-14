rainbow_hcl <- function(n, c = 50, l = 70, start = 0, end = 360*(n-1)/n,
                        gamma = NULL, fixup = TRUE, ...)
{
    if (!is.null(gamma))
        warning("'gamma' is deprecated and has no effect")
    if(n > 0)
        hex(polarLUV(L = l, C = c, H = seq(start, end, length = n)),
            fixup = fixup, ...)
    else
        character(0)
}

diverge_hcl <- function(n, h = c(260, 0), c = 80, l = c(30, 90), power = 1.5,
                        gamma = NULL, fixup = TRUE, ...)
{
    if (!is.null(gamma))
        warning("'gamma' is deprecated and has no effect")
    if(n < 1) return(character(0))
    h <- rep(h, length.out = 2)
    c <- c[1]
    l <- rep(l, length.out = 2)
    power <- rep(power, length.out = 2)
    rval <- seq(1, -1, length = n)
    rval <- hex(polarLUV(L = l[2] - diff(l) * abs(rval)^power[2],
                         C = c * abs(rval)^power[1],
                         H = ifelse(rval > 0, h[1], h[2])),
                fixup = fixup, ...)
    return(rval)
}

diverge_hsv <- function(n, h = c(240, 0), s = 1, v = 1, power = 1,
                        gamma = NULL, fixup = TRUE, ...)
{
    if (!is.null(gamma))
        warning("'gamma' is deprecated and has no effect")
    if(n < 1) return(character(0))
    h <- rep(h, length.out = 2)
    s <- s[1]
    v <- v[1]
    power <- power[1]
    rval <- seq(-s, s, length = n)
    rval <- hex(as(HSV(H = ifelse(rval > 0, h[2], h[1]),
                       S = abs(rval)^power, V = v, ...), "RGB"),
                fixup = fixup, ...)
    return(rval)
}

sequential_hcl <- function(n, h = 260, c. = c(80, 0), l = c(30, 90),
                           power = 1.5, gamma = NULL, fixup = TRUE, ...)
{
    if (!is.null(gamma))
        warning("'gamma' is deprecated and has no effect")
    if(n < 1) return(character(0))
    c <- rep(c., length.out = 2)
    l <- rep(l, length.out = 2)
    power <- rep(power, length.out = 2)
    rval <- seq(1, 0, length = n)
    rval <- hex(polarLUV(L = l[2] - diff(l) * rval^power[2],
                         C = c[2] - diff(c) * rval^power[1],
                         H = h[1]),
                fixup = fixup, ...)
    return(rval)
}

heat_hcl <- function(n, h = c(0, 90), c. = c(100, 30), l = c(50, 90),
                     power = c(1/5, 1), gamma = NULL, fixup = TRUE, ...)
{
    if (!is.null(gamma))
        warning("'gamma' is deprecated and has no effect")
    if(n < 1) return(character(0))
    h <- rep(h, length.out = 2)
    c <- rep(c., length.out = 2)
    l <- rep(l, length.out = 2)
    power <- rep(power, length.out = 2)
    rval <- seq(1, 0, length = n)
    rval <- hex(polarLUV(L = l[2] - diff(l) * rval^power[2],
                         C = c[2] - diff(c) * rval^power[1],
                         H = h[2] - diff(h) * rval),
                fixup = fixup, ...)
    return(rval)
}

terrain_hcl <- function(n, h = c(130, 0), c. = c(80, 0), l = c(60, 95),
                        power = c(1/10, 1), gamma = NULL, fixup = TRUE, ...)
{
    if (!is.null(gamma))
        warning("'gamma' is deprecated and has no effect")
    heat_hcl(n, h = h, c. = c., l = l, power = power,
             fixup = fixup, ...)
}
