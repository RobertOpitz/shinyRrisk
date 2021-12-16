#' @name buildTornadoPlot
#' @title buildTornadoPlot
#' @description Builds tornado graph for sensitivity analysis.
#' @param type Plot type. Possible values: relaxed, full.
#' @return Plotly plot object.
#' @export
buildTornadoPlot <- function(type) {
  cat("buildTornadoPlot\n")
  
  plotsData <- getTornadoPlotData(shiny::getDefaultReactiveDomain()$userData$model())
  plotData  <- plotsData[[type]]
  
  trace1 <- list(type = "bar", 
                 x = plotData$data[[1]], 
                 y = factor(rownames(plotData$data), 
                            levels = c(as.character(rownames(plotData$data)))),
                 orientation = "h")

  layout <- list(title    = plotData$main, 
                 xaxis    = list(type      = "linear", 
                                 range     = plotData$xlim,
                                 autorange = TRUE), 
                 yaxis    = list(type      = "category", 
                                 autorange = TRUE), 
                 autosize = TRUE)

  p <- createPlotlyWithConfig()

  p <- add_trace(p, 
                 name        = plotData$main, 
                 type        = trace1$type, 
                 x           = trace1$x, 
                 y           = trace1$y, 
                 autobinx    = trace1$autobinx, 
                 autobiny    = trace1$autobiny, 
                 orientation = trace1$orientation)
  
  p <- layout(p, 
              title      = layout$title, 
              xaxis      = layout$xaxis, 
              yaxis      = layout$yaxis, 
              autosize   = layout$autosize, 
              showlegend = layout$showlegend)

  p
}
