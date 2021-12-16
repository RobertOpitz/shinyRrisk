#' @name getItemRoleCodes
#' @title getItemRoleCodes
#' @description Builds a vector containing item roll codes.
#' @return Vector of item role codes.
#' @export
getItemRoleCodes <- function() {
  cat("getItemRoleCodes\n")
  c("nd" = "Not defined (nd)",
    "u"  = "This variable represents uncertainty only (u)",
    "uv" = "This variable represents both uncertainty and variability (uv)",
    "v"  = "This variable represents variability only (v)",
    "OF" = "This item has been defined as outcome function (OF)")
}
