#' @name prepareUncertaintiesPlotData
#' @title prepareUncertaintiesPlotData
#' @description Builds data for uncertainty items overview by setting up an uncertainty matrix for get function.
#' @param rriskModel Model object.
#' @return List of plot data.
#' @export
prepareUncertaintiesPlotData <- function(rriskModel) {
  cat("prepareUncertaintiesPlotData\n")

  modelUncertainties <- rriskModel@uncertainties@uncertainties
  modelSsystem       <- rriskModel@scoring
  useNotApplicable   <- rriskModel@settings@usenotapplicable
    
  if (length(modelUncertainties) == 0) {
    warning(paste("Model uncertainties list is empty -->", 
                  "No model uncertainties graph can be exported!\n"),
            immediate. = TRUE)
    return(NULL)
  }
      
  uncertParts <- c()
  for (uncert in modelUncertainties)
    if (!uncert@namemain %in% uncertParts)
      uncertParts <- c(uncertParts, uncert@namemain)
        
  plotsData <- vector(mode = "list", length = length(uncertParts))
  for (i in seq_along(uncertParts)) {
        
    uncertList <- list()
    for (uncert in modelUncertainties)
      if (uncert@namemain == uncertParts[i])
        uncertList <- c(uncertList, uncert)

    if (checkMatrixDataFit(modelUncertainties, modelSsystem)) {
      uncertMatrix <- uncertainties2matrix(uncertList,
                                           modelSsystem = modelSsystem)
              
      plotData <- getUncertaintiesPlotData(uncertMatrix     = uncertMatrix,
                                           uncertType       = uncertParts[i],
                                           modelSsystem     = modelSsystem,
                                           useNotApplicable = useNotApplicable,
                                           modelName        = rriskModel@name@name)

      plotsData[[i]] <- plotData
    }
  }
      
  plotsData
}
