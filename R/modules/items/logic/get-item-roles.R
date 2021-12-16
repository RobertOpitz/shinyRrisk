#' @name getItemRoles
#' @title getItemRoles
#' @description Builds a vector containing item rolls.
#' @return Vector of item roles.
#' @export
getItemRoles <- function() {
  cat("getItemRoles\n")
  c("Not defined (nd)"                                               = "nd",
    "This variable represents uncertainty only (u)"                  = "u",
    "This variable represents both uncertainty and variability (uv)" = "uv",
    "This variable represents variability only (v)"                  = "v",
    "This item has been defined as outcome function (OF)"            = "OF")
}
