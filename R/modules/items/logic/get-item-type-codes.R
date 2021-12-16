#' @name getItemTypeCodes
#' @title getItemTypeCodes
#' @description Builds a list containing item tool tips.
#' @return List of item tool tips.
#' @export
getItemTypeCodes <- function() {
  cat("getItemTypeCodes\n")
  c("",
    "data"     = "Data item (data)",
    "numv"     = "Numerical value(s) (numv)",
    "mcrv"     = "Monte-Carlo random variate (mcrv)",
    "fnrv"     = "Function of mcrv and other item(s) (fnrv)",
    "rsrv"     = "Resampling item (rsrv)",
    "param"    = "Bootstrap item - Parametric (bsrv)",
    "nonparam" = "Bootstrap item - Nonparametric (bsrv)")
}
