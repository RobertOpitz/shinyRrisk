#' @name serverAnalysis
#' @title serverAnalysis
#' @description Provides event handler functions.
#' @param id Namespace identifier.
#' @return NULL
#' @export
serverAnalysis <- function(id = "analysis") {
  cat("serverAnalysis\n")

  ns <- NS(id)

  module <- function(input, output, session) {

    observe({
      plotsData <- getTornadoPlotData(shiny::getDefaultReactiveDomain()$userData$model())
      # output should be actually a list, with each entry of the list contains
      # two lists: "relax", and "full"

      plotData <- plotsData[["relax"]]
      if (!is.null(plotData)) {
        output$pltTornadoSensitivityAnalysisRelaxed <- renderPlotly(buildTornadoPlot("relax"))
        shinyjs::show("pltTornadoSensitivityAnalysisRelaxed")
      } else 
        shinyjs::hide("pltTornadoSensitivityAnalysisRelaxed")

      plotData <- plotsData[["full"]]
      if (!is.null(plotData)) {
        output$pltTornadoSensitivityAnalysisFull <- renderPlotly(buildTornadoPlot("full"))
        shinyjs::show("pltTornadoSensitivityAnalysisFull")
      } else 
        shinyjs::hide("pltTornadoSensitivityAnalysisFull")
    })

    output$uioRegressionPlots <- renderUI({
      plotsData <- getGAMPlotData(session$userData$model())
      n <- length(plotsData)

      tagList({
        lapply(seq_along(plotsData), 
               function(i) {
                 plot1dName <- ns(paste0('pltRegression1dItem', i))
                 plot2dName <- ns(paste0('pltRegression2dItem', i))
                 tagList(fluidRow(column(width = 6,
                                         plotOutput(plot1dName)),
                                  column(width = 6,
                                         plotOutput(plot2dName))),
                         {if (i < n) hr()})})
      })
    })

    output$pltRegression1dItem1 <- renderPlot({

      plotsData <- getGAMPlotData(session$userData$model())
      
      createPlot <- function(pd, i) {
          plot1dName <- paste0('pltRegression1dItem', i)
          plot2dName <- paste0('pltRegression2dItem', i)
          
          output[[plot1dName]] <- renderPlot({
            p <- plot(pd$`1d`$gam,
                      main      = pd$`1d`$main,
                      shade     = pd$`1d`$shade,
                      xlab      = pd$`1d`$xlab,
                      ylab      = pd$`1d`$ylab,
                      residuals = pd$`1d`$residuals,
                      rug       = pd$`1d`$rug,
                      shade.col = pd$`1d`$shade.col)
          })
          
          output[[plot2dName]] <- renderPlot({
            p <- plot(pd$`2d`$gam,
                      main      = pd$`2d`$main,
                      shade     = pd$`2d`$shade,
                      xlab      = pd$`2d`$xlab,
                      ylab      = pd$`2d`$ylab,
                      residuals = pd$`2d`$residuals,
                      rug       = pd$`2d`$rug,
                      shade.col = pd$`2d`$shade.col)
          })
          
          output 
      }
      mapply(FUN = createPlot, plotsData, seq_along(plotsData))
    })
  }
  
  moduleServer(id, module)
}
