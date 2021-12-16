#' @name getProbabilityDensityValues
#' @title getProbabilityDensityValues
#' @description Builds a list containing probability density values.
#' @return List of probability density values.
#' @export
getProbabilityDensityValues <- function(distribution_name) {
  cat("getProbabilityDensityValues\n")
  switch(distribution_name,
         "bern"      = c("Prob (0 < prob < 1)"),
         "binom"     = c("Size (integer value)", "Prob (0 < prob < 1)"),
         "multinom"  = c("Prob. vector (0 < prob < 1), adds up to one"),
         "geom"      = c("Prob (0 < prob < 1)"),
         "hyper"     = c("m (integer value)", "n (integer value)", "k (integer value)"),
         "nbinom"    = c("Size (integer value)", "Prob (0 < prob < 1)"),
         "pois"      = c("Lambda (positive)"),
         "udiscrete" = c("Min (integer)", "Max (integer)"),
         "beta"      = c("Shape 1 (positive)", "Shape 2 (positive)"),
         "cauchy"    = c("Location", "Scale (positive)"),
         "chisq"     = c("Location (positive integer)"),
         "chisqnc"   = c("Df (positive)", "Ncp (positive)"),
         "exp"       = c("Rate (positive)"),
         "f"         = c("Df1 (positive)", "Df2 (positive)"),
         "gamma"     = c("Shape (positive)", "Rate (positive)"),
         "logis"     = c("Location", "Scale (positive)"),
         "lnorm"     = c("Meanlog", "Sdlog (positive)"),
         "norm"      = c("Mean", "Sd (positive)"),
         "tnorm"     = c("Mean", "Sd (positive)", "Lower (< mean)", "Upper (> mean)"),
         "t"         = c("t (positive)"),
         "triang"    = c("Min", "Mode (> min)", "Max (> mode)"),
         "unif"      = c("Min", "Max"),
         "weibull"   = c("Shape (positive)", "Scale (positive)"),
         "pert"      = c("Min", "Mode (> min)", "Max (> mode)"),
         "modpert"   = c("Min", "Mode (> min)", "Max (> mode)", "Shape"),
         "gompertz"  = c("Shape (positive)", "Scale (positive)"))
}
