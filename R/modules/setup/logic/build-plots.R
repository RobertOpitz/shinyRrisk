#' @name buildPlots
#' @title buildPlots
#' @description Builds all result plots for the model.
#' @return List of export plots.
#' @export
buildPlots <- function() {
  cat("buildPlots\n")
  model <- shiny::getDefaultReactiveDomain()$userData$model()
  
  plots <- list(plotCdf            = file.path(tempdir(), "plot-cdf.png"),
                plotTornado        = file.path(tempdir(), "plot-tornado.png"),
                plotHistogram      = file.path(tempdir(), "plot-histogram.png"),
                plotConvergence    = file.path(tempdir(), "plot-convergence.png"),
                plotUncertainties1 = file.path(tempdir(), "plot-uncertainties_1.png"),
                plotUncertainties2 = file.path(tempdir(), "plot-uncertainties_2.png"),
                plotUncertainties3 = file.path(tempdir(), "plot-uncertainties_3.png"))

  ggsave(file   = plots$plotCdf, 
         plot   = plotCDF(rriskModel = model), 
         device = "png")
  
  ggsave(file   = plots$plotTornado, 
         plot   = plotTornado(rriskModel = model), 
         device = "png")
  
  ggsave(file   = plots$plotHistogram, 
         plot   = plotOFHistogram(rriskModel = model), 
         device = "png")
  
  ggsave(file   = plots$plotConvergence, 
         plot   = plotOFConvergence(rriskModel = model), 
         device = "png")
  
  plotsData <- prepareUncertaintiesPlotData(rriskModel = model)
  for (i in seq_along(plotsData))
    ggsave(file   = plots[[paste0("plotUncertainties", i)]],
           plot   = buildUncertaintiesPlot(plotsData[[i]]),
           device = "png")

  plots
}
