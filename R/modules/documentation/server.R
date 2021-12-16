#' @name serverBasics
#' @title serverBasics
#' @description Provides event handler functions.
#' @param id Namespace identifier.
#' @return NULL
#' @export
serverBasics <- function(id = "basics") {
  cat("serverBasics\n")

  module <- function(input, output, session) {

    observe({
      updateTextInput(session, 
                      "txaMainDoc", 
                      value = session$userData$model()@basics@basics[[1]]@explanation)
      updateTextInput(session, 
                      "txaBackground", 
                      value = session$userData$model()@basics@basics[[2]]@explanation)
      updateTextInput(session, 
                      "txaObjectives", 
                      value = session$userData$model()@basics@basics[[3]]@explanation)
      updateTextInput(session, 
                      "txaScope", 
                      value = session$userData$model()@basics@basics[[4]]@explanation)
      updateTextInput(session, 
                      "txaDescription", 
                      value = session$userData$model()@basics@basics[[5]]@explanation)
    })

    observeEvent(input$btnSave, {
      model <- session$userData$model()

      model@basics@basics[[1]]@explanation <- input$txaMainDoc
      model@basics@basics[[2]]@explanation <- input$txaBackground
      model@basics@basics[[3]]@explanation <- input$txaObjectives
      model@basics@basics[[4]]@explanation <- input$txaScope
      model@basics@basics[[5]]@explanation <- input$txaDescription

      session$userData$model(model)

      shinyAlert(title = "Documentation saved", type = "success")
    })
  }
  
  moduleServer(id, module)
}
