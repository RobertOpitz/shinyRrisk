#' @name serverSummary
#' @title serverSummary
#' @description Provides event handler functions.
#' @param id Namespace identifier.
#' @return NULL
#' @export
serverSummary <- function(id = "summary") {
  cat("serverSummary\n")

  module <- function(input, output, session) {

    observe({
      if (!is.null(session$userData$model()@output@summaries))
        shinyjs::runjs("document.querySelectorAll('.shiny-spinner-output-container').forEach(e => e.classList.remove('d-none'))")
      else
        shinyjs::runjs("document.querySelectorAll('.shiny-spinner-output-container').forEach(e => e.classList.add('d-none'))")
    })

    output$tblResultsSummeries <- renderTable(
      format(session$userData$model()@output@summaries, scientific = TRUE),
      striped  = FALSE,
      hover    = FALSE,
      bordered = TRUE,
      rownames = TRUE,
      colnames = FALSE)

    output$pltCdf <- renderPlot(plotCDF(rriskModel = session$userData$model()))
    
    output$pltHistogram <- renderPlot(plotOFHistogram(rriskModel = session$userData$model()))

    output$pltConvergence <- renderPlot(plotOFConvergence(rriskModel = session$userData$model()))
  }
  
  moduleServer(id, module)
}
