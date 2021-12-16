#' @name filterPlotData
#' @title filterPlotData
#' @description Filters plot data for uncertainty plots.
#' @param uncertType Uncertainty type.
#' @return Plot data.
#' @export
filterPlotData <- function(plotsData, uncertType) {
  cat("filterPlotData\n")
  
  uncertTypeId <- getUncertaintyTypes()[[uncertType]] # ugly!
  plotData <- NULL
  for (this_plotData in plotsData) {
    if (identical(this_plotData$uncertTypeId, uncertTypeId)) {
      plotData <- this_plotData
      break
    }
  }
    
  plotData
}