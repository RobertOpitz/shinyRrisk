#' @name parseActionButton
#' @title parseActionButton
#' @description Parses action button id to integer.
#' @param idstr Identifier as string
#' @return Action Button index.
#' @export
parseActionButton <- function(idstr) {
  cat("parseActionButton ")
  id <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  cat("idstr =", idstr, "; id = ", id, "\n")

  if (!is.na(id))
    return(id) # else?
}
