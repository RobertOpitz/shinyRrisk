#' @name getObjectName
#' @title getObjectName
#' @description Gets an objects name.
#' @param object Object to read name of.
#' @return Object name as string.
#' @export
getObjectName <- function(object) {
  cat("getObjectName\n")
  deparse(substitute(object))
}
