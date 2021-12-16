#' @name buildUncertaintiesPlot
#' @title buildUncertaintiesPlot
#' @description Builds graph for uncertainty overview.
#' @param plotData Plot data.
#' @return Plot object.
#' @export
buildUncertaintiesPlot <- function(plotData) {
  cat("buildUncertaintiesPlot")
  
  p <- plot(plotData$data,
            ylim = plotData$ylim,
            main = plotData$main,
            type = plotData$type,
            axes = plotData$axes,
            xlab = plotData$xlab,
            ylab = plotData$ylab)

  # print dotted lines
  for (i in seq_len(plotData$matrixRowNum))
    lines(x   = c(1.2, 1.05 * plotData$matrixColNum),
          y   = c(i, i), 
          lty = "dotted")
  
  # print colored dots
  points(x   = plotData$x,
         y   = plotData$y, 
         cex = 4, 
         pch = 21, 
         col = plotData$colVector, 
         bg = plotData$colVector)
  
  # print axis description
  text(x      = seq_len(plotData$matrixColNum) + .2, 
       y      = rep(plotData$max.y, plotData$matrixColNum), 
       labels = plotData$matrixColNames)
  text(x      = rep(plotData$max.x - 2, plotData$matrixRowNum), 
       y      = plotData$matrixRowNum:1, 
       labels = plotData$matrixRowNames, 
       adj    = c(0, NA))

  # top line
  lines(x = c(0.8, plotData$max.x),
        y = c(plotData$max.y, plotData$max.y) - 0.5)
  
  # bottom line
  lines(x = c(0.8, plotData$max.x),
        y = c(0.5, 0.5))

  # left
  lines(x = rep(0.8, 2),
        y = c(plotData$max.y - 0.5, 0.5))

  # right
  lines(x = rep(plotData$max.x, 2),
        y = c(plotData$max.y - 0.5, 0.5))
  
  # print legend
  legend(min(plotData$y),
         0.3,
         pch       = 21,
         col       = unique(names(plotData$vcolors)),
         pt.bg     = unique(names(plotData$vcolors)),
         x.intersp = 2,
         horiz     = TRUE,
         pt.cex    = 4,
         bty       = "n",
         legend    = names(getScoringMeaningsBasic()),
         border    = FALSE)
  
  p
}
