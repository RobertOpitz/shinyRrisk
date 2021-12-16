#' @name buildNetworkPlot
#' @title buildNetworkPlot
#' @description Builds network graph for item adjacency.
#' @return Networkd3 plot object.
#' @export
buildNetworkPlot <- function(plotData) {
  cat("buildNetworkPlot\n")
  
  if (is.null(plotData))
    return(NULL)

  # get targets for links of forceNetwork
  #n <- nrow(plotData$ma)
  #target <- NULL
  #for (irow in seq_len(n))
  #  if (any(plotData$ma[irow,] > 0))
  #    target <- c(target, which(plotData$ma[irow,] > 0) - 1)
  
  # (works only if each source has only one target)
  n <- nrow(plotData$ma)
  target <- seq(from = 0, to = n - 1) # initially, all nodes point to themself
  for (irow in seq_len(n))
    if (any(plotData$ma[irow,] > 0))
      target[irow] <- which(plotData$ma[irow,] > 0) - 1
  
  # build data frames
  m <- length(target)
  links <- data.frame(source = seq(from = 0, to = m - 1), # from node
                      target = target,                    # to node
                      value  = rep(5, m))
  nodes <- data.frame(node_id = rownames(plotData$ma), 
                      group   = plotData$group, 
                      size    = rep(12, n))

  # return net graph
  forceNetwork(Links          = links,
               Nodes          = nodes,
               Source         = "source",
               Target         = "target",
               Value          = "value",
               NodeID         = "node_id",
               Group          = "group",
               fontSize       = 12,
               fontFamily     = "Arial",
               arrows         = TRUE,
               zoom           = TRUE,
               opacityNoHover = 1,
               opacity        = 1,
               legend         = TRUE)
}
