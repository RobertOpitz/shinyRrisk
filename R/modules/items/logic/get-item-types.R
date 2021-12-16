#' @name getItemTypes
#' @title getItemTypes
#' @description Builds a list containing item types.
#' @return List of item types.
#' @export
getItemTypes <- function() {
  cat("getItemTypes\n")
  c("",
    "Data item (data)"                          = "data",
    "Numerical value(s) (numv)"                 = "numv",
    "Monte-Carlo random variate (mcrv)"         = "mcrv",
    "Function of mcrv and other item(s) (fnrv)" = "fnrv",
    "Resampling item (rsrv)"                    = "rsrv",
    "Bootstrap item - Parametric (bsrv)"        = "param",
    "Bootstrap item - Nonparametric (bsrv)"     = "nonparam")
}
