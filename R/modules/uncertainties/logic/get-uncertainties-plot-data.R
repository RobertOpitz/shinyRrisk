#' @name getUncertaintiesPlotData
#' @title getUncertaintiesPlotData
#' @description Builds data for uncertainty items overview.
#' @param uncertMatrix Matrix of uncertainties.
#' @param uncertType Uncertainty type.
#' @param modelSsystem Scoring system object.
#' @param useNotApplicable Boolean to determine if value "not applicable" to be used.
#' @param modelName Model name.
#' @return List of plot data.
#' @export
getUncertaintiesPlotData <- function(uncertMatrix, uncertType, 
                                     modelSsystem, useNotApplicable = TRUE, 
                                     modelName = NULL) {
  cat("getUncertaintiesPlotData\n")
  
  #print(uncertMatrix)
  
  # define help variables
  vcolors     <- modelSsystem@vcolors
  vmeanings   <- modelSsystem@vmeanings
    
  # remove columns with all not-applicable entries
  if (useNotApplicable == FALSE) {
    notapplicable <- vmeanings[which(names(vmeanings) == "notapplicable")]
    
    colToRemove <- c()
    for (i in seq_len(ncol(uncertMatrix))) {
      if (all(uncertMatrix[, i] == notapplicable)) 
        colToRemove <- c(colToRemove, i)    
    } 
    print(colToRemove)
    test <- apply(uncertMatrix, 2, 
                  function(column) all(column == notapplicable))
    print(test)

    uncertMatrix <- uncertMatrix[, -colToRemove]
    vcolors      <- vcolors[which(vcolors != notapplicable)]
    vmeanings    <- vmeanings[which(vmeanings != notapplicable)]
  }
    
  matVector <- as.vector(t(uncertMatrix))
  colVector <- rep(NA, length(matVector)) # ? length(matVecor)  != length(vcolors)?
  color_names <- names(vcolors)
  for (i in seq_along(vcolors)) {
      colVector[which(matVector == vcolors[i])] <- color_names[i]
  }
    
  matrixRows <- nrow(uncertMatrix)
  matrixCols <- ncol(uncertMatrix)
  y <- rep(matrixRows:1, each = matrixCols)
  x <- rep(1:matrixCols, matrixRows) + .2

  max.x <- max(x) + 3
  max.y <- max(y) + 1
    
  plotData <- list("data"           = c(1, max.y) ~ c(1, max.x), 
                   "ylim"           = c(0, max.y), 
                   "main"           = uncertType,
                   "type"           = "n", 
                   "axes"           = FALSE, 
                   "xlab"           = "", 
                   "ylab"           = "",
                   "x"              = x,
                   "y"              = y,
                   "max.x"          = max.x,
                   "max.y"          = max.y,
                   "matrixRowNum"   = matrixRows,
                   "matrixColNum"   = matrixCols,
                   "matrixRowNames" = rownames(uncertMatrix),
                   "matrixColNames" = colnames(uncertMatrix),
                   "colVector"      = colVector,
                   "vcolors"        = vcolors,
                   "vmeanings"      = vmeanings,
                   "uncertTypeId"   = getUncertaintyTypes()[[uncertType]]) # ugly 
    
  plotData
}
