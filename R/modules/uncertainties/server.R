#' @name serverUncertainties
#' @title serverUncertainties
#' @description Provides event handler functions.
#' @param id Namespace identifier.
#' @return NULL
#' @export
serverUncertainties <- function(id = "uncertainties") {
  cat("serverUncertainties\n")
  
  ns <- NS(id)
  
  # internal helper function
  get_seq <- function(x) seq_len(x[length(x)])

  module <- function(input, output, session) {
    
    #---BEGIN: Functionality for uncertainty plots------------------------------
    session$userData$uncertPlotRowHeight    <- reactiveVal(75)
    session$userData$uncertPlotExtraHeight  <- reactiveVal(100)
    
    observe({
      # show / hide uncertainty plots
      plotsData <- prepareUncertaintiesPlotData(shiny::getDefaultReactiveDomain()$userData$model())
      
      # modeling approach uncertainties
      plotData  <- filterPlotData(plotsData, 
                                  getUncertaintyTypes()[["modelling approach uncertainties"]]) # ugly
      updateNumericInput(session = session, 
                         inputId = "nuiModellingUncertY", 
                         value   = plotData$max.y * session$userData$uncertPlotRowHeight() + session$userData$uncertPlotExtraHeight())
      
      if (!is.null(plotData))
        shinyjs::show("pltUncertaintiesModellingUncert")
      else 
        shinyjs::hide("pltUncertaintiesModellingUncert")
      
      # scenario uncertainties
      plotData <- filterPlotData(plotsData,
                                 getUncertaintyTypes()[["scenario uncertainties"]]) # ugly
      updateNumericInput(session = session, 
                         inputId = "nuiScenarioUncertY", 
                         value = plotData$max.y * session$userData$uncertPlotRowHeight() + session$userData$uncertPlotExtraHeight())
      
      if (!is.null(plotData))
        shinyjs::show("pltUncertaintiesScenarioUncert")
      else 
        shinyjs::hide("pltUncertaintiesScenarioUncert")
      
      # other uncertainties
      plotData <- filterPlotData(plotsData, 
                                 getUncertaintyTypes()[["other uncertainties"]]) # ugly
      updateNumericInput(session = session, 
                         inputId = "nuiOtherUncertY", 
                         value = plotData$max.y * session$userData$uncertPlotRowHeight() + session$userData$uncertPlotExtraHeight())
      
      if (!is.null(plotData))
        shinyjs::show("pltUncertaintiesOtherUncert")
      else 
        shinyjs::hide("pltUncertaintiesOtherUncert")
      
      # show / hide network plot
      plotData <- getModelNetworkPlotData(shiny::getDefaultReactiveDomain()$userData$model())
      if (!is.null(plotData))
        shinyjs::show("pltD3NetworkGraph")
      else 
        shinyjs::hide("pltD3NetworkGraph")
    })
    
    local_renderPlot <- function(which_uncert_plot, height) {
      renderPlot(
        {
          plotsData <- prepareUncertaintiesPlotData(shiny::getDefaultReactiveDomain()$userData$model())
          plotData  <- filterPlotData(plotsData,
                                      getUncertaintyTypes()[[which_uncert_plot]]) # ugly
          if (!is.null(plotData))
            buildUncertaintiesPlot(plotData)
        },
        height = function() height
      )
    }
    output$pltUncertaintiesModellingUncert <- local_renderPlot("modelling approach uncertainties",
                                                              input$nuiModellingUncertY)
    output$pltUncertaintiesScenarioUncert  <- local_renderPlot("scenario uncertainties",
                                                              input$nuiScenarioUncertY)
    output$pltUncertaintiesOtherUncert     <- local_renderPlot("other uncertainties",
                                                               input$nuiOtherUncertY)
    #---END: functionality for uncertainty plots--------------------------------

    observe(displayUncertainties(output, ns))

    observeEvent(input$btnOpenModal, {
      model        <- session$userData$model()
      sv           <- model@scoring@values
      scoringItems <- model@scoring@scoring
      
      cat("serverUncertainties -> observe event open modal\n")
      print(getScoringMeaningsDetailed()[get_seq(sv)]) # ugly
      
      showModal(uncertaintyModalAdd(
        scoringItems = scoringItems,
        possibleScorings = getScoringMeaningsDetailed()[get_seq(sv)] # ugly
      ))
    })

    observeEvent(input$btnAdd, {

      if (!validateUncertaintyModalAdd(input))
        return(NULL)

      nextRow <- length(session$userData$model()@uncertainties@uncertainties) +1

      setUncertainty(input, nextRow)
    })

    observeEvent(input$btnModalEdit, {
      model            <- session$userData$model()
      sv               <- model@scoring@values
      scoringItems     <- model@scoring@scoring
      rowNum           <- parseActionButton(input$btnModalEdit)
      uncertaintyScore <- session$userData$model()@uncertainties@uncertainties[[rowNum]]
      
      cat("serverUncertainties->observe event modal edit\n")
      #print(getScoringMeaningsDetailed(get_seq(sv)))
      
      showModal(
        uncertaintyModalEdit(
          scoringItems     = scoringItems,
          possibleScorings = getScoringMeaningsDetailed()[get_seq(sv)], #ugly
          uncertaintyScore = uncertaintyScore,
          rowNum           = rowNum))
    })

    observeEvent(input$btnEdit, {
      if (!validateUncertaintyModalEdit(input))
        return(NULL)
      setUncertainty(input, strtoi(input$txbRowNum))
    })

    observeEvent(input$btnDelete, {
      rowNum                            <- parseActionButton(input$btnDelete)
      model                             <- session$userData$model()
      model@uncertainties@uncertainties <- model@uncertainties@uncertainties[-rowNum]

      session$userData$model(model)
    })

    observeEvent(input$btnReset, {
      model               <- session$userData$model()
      model@uncertainties <- initUncertainties()

      session$userData$model(model)

      displayUncertainties(output, ns)
    })
  }

  moduleServer(id, module)
}

