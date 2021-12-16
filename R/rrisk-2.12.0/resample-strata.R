#' @name resampleSTRATA
#' @aliases resampleSTRATA
#' @title Non-executable auxiliary function
#' @usage resampleSTRATA(rriskModel)
#' @param rriskModel  ...
#' @keywords run
#' @export

resampleSTRATA <- function(rriskModel) {
  cat("resampleSTRATA\n")
  
  items <- rriskModel@items@items
  # loop over all items
  for (i in seq_along(items)) {
    if (items[[i]]@typecode == "stra") {
      stratumData             <- list(items[[i]]@data)
      names(stratumData)[[1]] <- items[[i]]@name
      items[[i]]@data$strv    <- with(stratumData,
                                      eval(parse(text = items[[i]]@fullc)))
    }
  }
  # save modified items to the model
  rriskModel@items@items <- items
  rriskModel
}