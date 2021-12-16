#' @name serverHeader
#' @title serverHeader
#' @description Provides event handler functions.
#' @param id Namespace identifier.
#' @return NULL
#' @export
serverHeader <- function(id = "header") {
  cat("serverHeader\n")
  ns <- NS(id)

  module <- function(input, output, session) {
    output$labActiveModelName <- renderText(session$userData$model()@name@name)
    output
  }
  
  moduleServer(id, module)
}
