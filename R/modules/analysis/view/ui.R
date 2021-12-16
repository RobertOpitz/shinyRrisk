#' @name uiAnalysis
#' @title uiAnalysis
#' @description Builds the UI of the sensitivity analysis context.
#' @param id Namespace identifier.
#' @return HTML source code.
#' @export
uiAnalysis <- function(id = "analysis") {
  cat("uiAnalysis\n")
  ns <- NS(id)
  
  tabItem(tabName = "analysis",
          fluidRow(box(title       = "Sensitivity Analysis",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       plotlyOutput(outputId = ns('pltTornadoSensitivityAnalysisFull')),
                       plotlyOutput(outputId = ns('pltTornadoSensitivityAnalysisRelaxed')))),
          fluidRow(box(title       = "Regression",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       uiOutput(ns("uioRegressionPlots")))))
}
