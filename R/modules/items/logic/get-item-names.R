#' @name getItemNames
#' @title getItemNames
#' @description Builds a vector containing files names of existing items.
#' @return Vector of item names.
#' @export
getItemNames <- function() {
  cat("getItemNames\n")

  session <- shiny::getDefaultReactiveDomain()
  
  sapply(session$userData$model()@items@items,
         function(item) item@name)
}
