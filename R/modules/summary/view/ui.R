#' @name uiSummary
#' @title uiSummary
#' @description Builds the UI of the summary context.
#' @param id Namespace identifier.
#' @return HTML source code.
#' @export
uiSummary <- function(id = "summary") {
  cat("uiSummary\n")
  ns <- NS(id)
  
  tabItem(tabName = "summary",
          fluidRow(box(title       = "Summary",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       shinycssloaders::withSpinner(
                         tableOutput(outputId = ns("tblResultsSummeries")))),
                   box(title       = "CDF",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       shinycssloaders::withSpinner(
                         plotOutput(outputId = ns("pltCdf")))),
                   box(title       = "Histogram",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       shinycssloaders::withSpinner(
                         plotOutput(outputId = ns("pltHistogram")))),
                   box(title       = "Convergence",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       shinycssloaders::withSpinner(
                         plotOutput(outputId = ns("pltConvergence"))))))
}
