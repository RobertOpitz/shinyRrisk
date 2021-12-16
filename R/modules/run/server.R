#' @name serverRun
#' @title serverRun
#' @description Provides event handler functions.
#' @param id Namespace identifier.
#' @return NULL
#' @export
serverRun <- function(id = "run") {
  cat("serverRun\n")
  ns <- NS(id)

  module <- function(input, output, session) {

    observe({
      model <- session$userData$model()
      updateRadioButtons(session  = session, 
                         inputId  = "rabRun", 
                         selected = "1d")
    })

    #---BEGIN observeEvent: button RUN------------------------------------------ 
    observeEvent(input$btnRun, {

      shinyjs::disable("btnRun")

      isOf    <- FALSE
      isUOrUv <- FALSE
      isV     <- FALSE

      for (item in session$userData$model()@items@items) {

        if (item@rolecode == "OF")
          isOf <- TRUE

        if (item@typecode == "mcrv") {
          if (item@rolecode == "u" | item@rolecode == "uv")
            isUOrUv <- TRUE
          else if (item@rolecode == "v")
            isV <- TRUE
        }
      }

      if (!isOf) {
        shinyAlert(title = "Please define at least on OF!", type = "error")
        shinyjs::enable("btnRun")
        return(NULL)
      }

      switch(input$rabRun,
             "1d" = {timer_start <- proc.time()[3]
                     session$userData$model(run1dsimulation(session$userData$model()))
                     shinyAlert(title   = "Done", 
                                message = paste("1d run completed after",
                                                round(proc.time()[3] - timer_start, 2),
                                                "seconds"))},
             "2d" = {if (!isUOrUv | !isV) {
                       shinyAlert(title = paste("Your need at least on item", 
                                                "with the role (u or uv) and", 
                                                "one with the role v!"), 
                                  type  = "error")
                       shinyjs::enable("btnRun")
                       return(NULL)
                     }
                     id <- as.integer(sub("^([0-9]+)-.*", "\\1", input$slbItems))
                     timer_start <- proc.time()[3]
                     session$userData$model(run2dsimulation(session$userData$model(), 
                                                            ofId = id))
                     shinyAlert(title   = "Done", 
                                message = paste("2d run completed after",
                                                round(proc.time()[3] - timer_start, 2),
                                                "seconds"))}
      )

      shinyjs::enable("btnRun")
    })
    #---END observeEvent: button RUN--------------------------------------------

    # Radio buttons for RUN (1d, 2d)
    observeEvent(input$rabRun, {
      
      shinyjs::hide("slbItems")

      if (input$rabRun != "2d")
        return(NULL)

      shinyjs::show("slbItems")

      items   <- session$userData$model()@items@items
      values  <- list()
      for (i in seq_along(items))
          if (items[[i]]@rolecode == "OF")
            values <- append(values, paste(i, items[[i]]@name, sep = "-"))

      updateSelectInput(session,
                        inputId = "slbItems",
                        choices = values,
                        selected = NULL)
    })
    
    # observe for text in RUN settings
    observe({
      updateTextInput(session = session, 
                      inputId = "txbN", 
                      value   = session$userData$model()@settings@N)
      updateTextInput(session = session, 
                      inputId = "txbN2d", 
                      value   = session$userData$model()@settings@N2d)
      updateTextInput(session = session, 
                      inputId = "txbAbserrors", 
                      value   = session$userData$model()@settings@abserror)
    })
    
    # observeEvent for save button of run settings
    observeEvent(input$btnSave, {
      
      hideFeedback(inputId = "txbN", session = session)
      hideFeedback(inputId = "txbN2d", session = session)
      hideFeedback(inputId = "txbAbserrors", session = session)
      
      if (!validateSimulationForm(input))
        return(NULL)
      
      model <- session$userData$model()
      model@settings <- new(Class    = "modelSettingsClass",
                            N        = input$txbN,
                            N2d      = input$txbN2d,
                            abserror = input$txbAbserrors)
      session$userData$model(model)
      
      shinyAlert(title = "Settings saved")
    })

    # observeEvent for restting results
    observeEvent(input$btnReset, {
      session$userData$model(resetSimulationResults(session$userData$model()))
      shinyAlert(title = "Reset successful")
    })
  }
  
  moduleServer(id, module)
}
