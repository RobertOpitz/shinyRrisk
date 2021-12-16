#' @name getModelNetworkPlotData
#' @title getModelNetworkPlotData
#' @description Builds data for item adjacency network graph.
#' @param rriskModel Model object.
#' @return List of plot data.
#' @export
getModelNetworkPlotData <- function(rriskModel) {
  cat("getModelNetworkPlot\n")

  # initialize help variables
  modelItems <- rriskModel@items@items

  len <- length(modelItems)
  if (len < 1)
    return(NULL)

  #  create model adjacency matrix
  modelAdjacency           <- matrix(0, nrow = len, ncol = len)
  itemNames                <- sapply(modelItems, function(item) item@name)
  colnames(modelAdjacency) <- itemNames
  rownames(modelAdjacency) <- itemNames

  for(i in seq_along(modelItems)) {
    depItems <- strsplit(modelItems[[i]]@depitem, split = " ") 
    if (length(depItems) > 0) {
      depItems <- depItems[[1]]
      depItems <- setdiff(depItems, "")
      depItems <- gsub(x = depItems, " ", replacement = "")
      modelAdjacency[i, which(depItems == itemNames)] <- 1
    }
  }

  itemGroup <- rep("NA", len)
  for (i in seq_len(len)) { 
    itemGroup[i] <- modelItems[[i]]@typecode
    if (modelItems[[i]]@rolecode == "OF") 
      itemGroup[i] <- paste(itemGroup[i], "(OF)")
  }

  list(ma    = modelAdjacency,
       group = itemGroup)
}
