getUncertaintiesPlotsForReport <- function(rriskModel) {
  cat("getUncertaintiesPlotsForReport\n")
  plotsData <- prepareUncertaintiesPlotData(rriskModel)
  
  lapply(plotsData, 
         function(pd) {
          if (is.null(pd) == FALSE)
            buildUncertaintiesPlot(pd)
         })
}