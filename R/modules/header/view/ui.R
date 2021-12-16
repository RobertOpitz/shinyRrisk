#' @name uiHeader
#' @title uiHeader
#' @description Builds the UI of the dashboard header.
#' @param id Namespace identifier.
#' @return HTML source code.
#' @export
uiHeader <- function(id = "header") {
  cat("uiHeader\n")
  ns <- NS(id)
  
  dashboardHeader(title   = "shinyRrisk",
                  disable = FALSE,
                  tags$li(class = "dropdown",
                          textOutput("labActiveModelName")))
}
