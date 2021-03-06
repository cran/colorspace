# All matrices in this file are adapted from https://github.com/njsmith/colorspacious/blob/master/colorspacious/cvd.py

#' Color Vision Deficiency (CVD) Conversion Tables
#' 
#' Conversion tables for simulating different types of color vision deficiency (CVD):
#' Protanomaly, deutanomaly, tritanomaly.
#' 
#' Machado et al. (2009) have established a novel model, that allows to handle normal color
#' vision, anomalous trichromacy, and dichromacy in a unified way. They also provide conversion
#' formulas along with tables of certain constants that allow to simulate various types of
#' CVD. See \code{\link{simulate_cvd}} for the corresponding simulation functions.
#' 
#' @name cvd
#' @rdname cvd
#' @format Lists of 3x3 RGB-color transformation matrices for the various types of CVD. Each list contains 11 transformation matrices 
#'   representing increasingly severe color vision deficiency.
#' @usage NULL
#' @seealso \code{\link{simulate_cvd}}
#' @references Machado GM, Oliveira MM, Fernandes LAF (2009).
#'   A Physiologically-Based Model for Simulation of Color Vision Deficiency.
#'   \emph{IEEE Transactions on Visualization and Computer Graphics}. \bold{15}(6), 1291--1298.
#'   \doi{10.1109/TVCG.2009.113}
#'   Online version with supplements at
#'   \url{http://www.inf.ufrgs.br/~oliveira/pubs_files/CVD_Simulation/CVD_Simulation.html}.
#'
#' Zeileis A, Fisher JC, Hornik K, Ihaka R, McWhite CD, Murrell P, Stauffer R, Wilke CO (2020).
#' \dQuote{colorspace: A Toolbox for Manipulating and Assessing Colors and Palettes.}
#' \emph{Journal of Statistical Software}, \bold{96}(1), 1--49. \doi{10.18637/jss.v096.i01}
NULL

#' @rdname cvd
#' @format NULL
#' @usage protanomaly_cvd
#' @export
protanomaly_cvd <- list(
  "0" = matrix(c(
    1.000000,  0.000000, -0.000000,
    0.000000,  1.000000,  0.000000,
    -0.000000, -0.000000,  1.000000
  ), 3,3,byrow = TRUE),
  "1" = matrix(c(
    0.856167,  0.182038, -0.038205,
    0.029342,  0.955115,  0.015544,
    -0.002880, -0.001563,  1.004443
  ),3,3,byrow=TRUE),
  "2" = matrix(c(
    0.734766,  0.334872, -0.069637,
    0.051840,  0.919198,  0.028963,
    -0.004928, -0.004209,  1.009137
  ),3,3,byrow=TRUE),
  "3" = matrix(c(
    0.630323,  0.465641, -0.095964,
    0.069181,  0.890046,  0.040773,
    -0.006308, -0.007724,  1.014032
  ),3,3,byrow=TRUE),
  "4" = matrix(c(
    0.539009,  0.579343, -0.118352,
    0.082546,  0.866121,  0.051332,
    -0.007136, -0.011959,  1.019095
  ),3,3,byrow=TRUE),
  "5" = matrix(c(
    0.458064,  0.679578, -0.137642,
    0.092785,  0.846313,  0.060902,
    -0.007494, -0.016807,  1.024301
  ),3,3,byrow=TRUE),
  "6" = matrix(c(
    0.385450,  0.769005, -0.154455,
    0.100526,  0.829802,  0.069673,
    -0.007442, -0.022190,  1.029632
  ),3,3,byrow=TRUE),
  "7" = matrix(c(
    0.319627,  0.849633, -0.169261,
    0.106241,  0.815969,  0.077790,
    -0.007025, -0.028051,  1.035076
  ),3,3,byrow=TRUE),
  "8" = matrix(c(
    0.259411,  0.923008, -0.182420,
    0.110296,  0.804340,  0.085364,
    -0.006276, -0.034346,  1.040622
  ),3,3,byrow=TRUE),
  "9" = matrix(c(
    0.203876,  0.990338, -0.194214,
    0.112975,  0.794542,  0.092483,
    -0.005222, -0.041043,  1.046265
  ),3,3,byrow=TRUE),
  "10" = matrix(c(
    0.152286,  1.052583, -0.204868,
    0.114503,  0.786281,  0.099216,
    -0.003882, -0.048116,  1.051998
  ),3,3,byrow=TRUE)

)

#' @rdname cvd
#' @format NULL
#' @usage deutanomaly_cvd
#' @export
deutanomaly_cvd <- list(
  "0" = matrix(c(
    1.000000,  0.000000, -0.000000,
    0.000000,  1.000000,  0.000000,
    -0.000000, -0.000000,  1.000000
  ),3,3,byrow=TRUE),
  "1" = matrix(c(
    0.866435,  0.177704, -0.044139,
    0.049567,  0.939063,  0.011370,
    -0.003453,  0.007233,  0.996220
  ),3,3,byrow=TRUE),
  "2" = matrix(c(
    0.760729,  0.319078, -0.079807,
    0.090568,  0.889315,  0.020117,
    -0.006027,  0.013325,  0.992702
  ),3,3,byrow=TRUE),
  "3" = matrix(c(
    0.675425,  0.433850, -0.109275,
    0.125303,  0.847755,  0.026942,
    -0.007950,  0.018572,  0.989378
  ),3,3,byrow=TRUE),
  "4" = matrix(c(
    0.605511,  0.528560, -0.134071,
    0.155318,  0.812366,  0.032316,
    -0.009376,  0.023176,  0.986200
  ),3,3,byrow=TRUE),
  "5" = matrix(c(
    0.547494,  0.607765, -0.155259,
    0.181692,  0.781742,  0.036566,
    -0.010410,  0.027275,  0.983136
  ),3,3,byrow=TRUE),
  "6" = matrix(c(
    0.498864,  0.674741, -0.173604,
    0.205199,  0.754872,  0.039929,
    -0.011131,  0.030969,  0.980162
  ),3,3,byrow=TRUE),
  "7" = matrix(c(
    0.457771,  0.731899, -0.189670,
    0.226409,  0.731012,  0.042579,
    -0.011595,  0.034333,  0.977261
  ),3,3,byrow=TRUE),
  "8" = matrix(c(
    0.422823,  0.781057, -0.203881,
    0.245752,  0.709602,  0.044646,
    -0.011843,  0.037423,  0.974421
  ),3,3,byrow=TRUE),
  "9" = matrix(c(
    0.392952,  0.823610, -0.216562,
    0.263559,  0.690210,  0.046232,
    -0.011910,  0.040281,  0.971630
  ),3,3,byrow=TRUE),
  "10" = matrix(c(
    0.367322,  0.860646, -0.227968,
    0.280085,  0.672501,  0.047413,
    -0.011820,  0.042940,  0.968881
  ),3,3,byrow=TRUE)
)

#' @rdname cvd
#' @format NULL
#' @usage tritanomaly_cvd
#' @export
tritanomaly_cvd <- list(
  "0" = matrix(c(
    1.000000,  0.000000, -0.000000,
    0.000000,  1.000000,  0.000000,
    -0.000000, -0.000000,  1.000000
  ),3,3,byrow=TRUE),
  "1" = matrix(c(
    0.926670,  0.092514, -0.019184,
    0.021191,  0.964503,  0.014306,
    0.008437,  0.054813,  0.936750
  ),3,3,byrow=TRUE),
  "2" = matrix(c(
    0.895720,  0.133330, -0.029050,
    0.029997,  0.945400,  0.024603,
    0.013027,  0.104707,  0.882266
  ),3,3,byrow=TRUE),
  "3" = matrix(c(
    0.905871,  0.127791, -0.033662,
    0.026856,  0.941251,  0.031893,
    0.013410,  0.148296,  0.838294
  ),3,3,byrow=TRUE),
  "4" = matrix(c(
    0.948035,  0.089490, -0.037526,
    0.014364,  0.946792,  0.038844,
    0.010853,  0.193991,  0.795156
  ),3,3,byrow=TRUE),
  "5" = matrix(c(
    1.017277,  0.027029, -0.044306,
    -0.006113,  0.958479,  0.047634,
    0.006379,  0.248708,  0.744913
  ),3,3,byrow=TRUE),
  "6" = matrix(c(
    1.104996, -0.046633, -0.058363,
    -0.032137,  0.971635,  0.060503,
    0.001336,  0.317922,  0.680742
  ),3,3,byrow=TRUE),
  "7" = matrix(c(
    1.193214, -0.109812, -0.083402,
    -0.058496,  0.979410,  0.079086,
    -0.002346,  0.403492,  0.598854
  ),3,3,byrow=TRUE),
  "8" = matrix(c(
    1.257728, -0.139648, -0.118081,
    -0.078003,  0.975409,  0.102594,
    -0.003316,  0.501214,  0.502102
  ),3,3,byrow=TRUE),
  "9" = matrix(c(
    1.278864, -0.125333, -0.153531,
    -0.084748,  0.957674,  0.127074,
    -0.000989,  0.601151,  0.399838
  ),3,3,byrow=TRUE),
  "10" = matrix(c(
    1.255528, -0.076749, -0.178779,
    -0.078411,  0.930809,  0.147602,
    0.004733,  0.691367,  0.303900
  ),3,3,byrow=TRUE)
)

