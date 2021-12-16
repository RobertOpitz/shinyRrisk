#' @name fitData
#' @title fitData
#' @description Fits datasets using a continuous function.
#' @param dataId List index of data items.
#' @param continuousFunction Continuous Function used to fit data.
#' @return Fitted dataset.
#' @export
fitData <- function(dataId, continuousFunction) {
  cat("fitData: dataId = ", dataId, 
      "; continuousFunction = ", continuousFunction, "\n")

  if (dataId == "" | continuousFunction == "")
    return(NULL)

  # seesion should be come from the input?
  session <- shiny::getDefaultReactiveDomain()

  availableData.data <- list()
  items              <- session$userData$model()@items@items

  for (item in items) {
    if (item@typecode == "data" && !is.null(item@data)) { # continuous overriding of variables?
      availableData.data  <- c(availableData.data, as.list(item@data))
    }
  }

  dataId   <- as.numeric(dataId)
  data2fit <- availableData.data[[dataId]]
  data2fit <- as.vector(unlist(data2fit)) # ? unlist is already a vector?

  fit.cont(data2fit           = data2fit, 
           continuousFunction = continuousFunction)
}
