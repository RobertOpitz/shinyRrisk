#' @name createPlotlyWithConfig
#' @title createPlotlyWithConfig
#' @description Creates a list of indices for all data items. Optionally the list also includes mrcv and fnrv items.
#' @return Plot.
#' @export
createPlotlyWithConfig <- function() {
  cat("createPlotlyWithConfig\n")
  config(plot_ly(), 
         displaylogo            = FALSE,
         modeBarButtonsToRemove = c("select2d",
                                    "lasso2d",
                                    "zoomIn2d", 
                                    "zoomOut2d",
                                    "autoScale2d",
                                    "toggleSpikelines",
                                    "hoverClosestCartesian", 
                                    "hoverCompareCartesian"))
}
