#' @name getItemSet
#' @title getItemSet
#' @description Creates a list of indices for all data items. Optionally the list also includes mrcv and fnrv items.
#' @param includeMcrvFnrv Boolean to additionally include mrcv and fnrv items.
#' @return List of item indices.
#' @export
getItemSet <- function(includeMcrvFnrv = FALSE) {
#getItemSet <- function(items, includeMcrvFnrv = FALSE) {
  
  cat("getItemSet\n")

  itemSet <- list()
  items   <- shiny::getDefaultReactiveDomain()$userData$model()@items@items
  index   <- 1
  
  for (item in items) {
    if (item@typecode == "data" & !is.null(item@data)) {
      availableData <- paste(item@name, names(item@data), sep = "$")
      for (data in availableData) {
        itemSet[[data]] <- index
        index <- index + 1
      }
    }
  }

  if (includeMcrvFnrv) {
    for (item in items) {
      if (item@typecode == "mcrv" | item@typecode == "fnrv") {
        itemSet[[item@name]] <- index
        index <- index + 1
      }
    }
  }

  itemSet
}
