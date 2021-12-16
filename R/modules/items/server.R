#' @title serverItems
#' @description Provides event handler functions.
#' @param id Namespace identifier.
#' @return NULL
#' @export
serverItems <- function(id = "items") {
  cat("serverItems\n")
  ns <- NS(id)

  module <- function(input, output, session) {
    
    #---BEGIN: plot DAG view of model-------------------------------------------
    observe({     
      # show / hide network plot
      model <- shiny::getDefaultReactiveDomain()$userData$model()
      plotData  <- getModelNetworkPlotData(model)
      if (is.null(plotData))
        shinyjs::hide("pltD3NetworkGraph")
      else {
        output$pltD3NetworkGraph <- renderForceNetwork(buildNetworkPlot(plotData))
        shinyjs::show("pltD3NetworkGraph")
      }
    })
    
    #---END: plot DAG view of model---------------------------------------------

    session$userData$resultMatrix <- reactiveVal()
    session$userData$pltEmpTheor  <- reactiveVal()
    session$userData$pltQq        <- reactiveVal()
    session$userData$pltCdf2      <- reactiveVal()
    session$userData$pltPp        <- reactiveVal()

    observeEvent(input$btnOpenItemModal, showModal(itemModalAdd()))

    observeEvent(input$slbRole, {
      if(tolower(input$slbRole) == "of") {
        updateSelectInput(session,
                          inputId  = "slbType",
                          choices  = c("Function of mcrv and other item(s) (fnrv)" = "fnrv"),
                          selected = "fnrv")
      } else
        updateSelectInput(session,
                          inputId  = "slbType",
                          choices  = getItemTypes(),
                          selected = input$slbType)
    })

    observeEvent(input$slbType, {
      disableAllSections()
      slb_type <- tolower(input$slbType)
      if(slb_type == "mcrv") {
        shinyjs::show("divItemProbabilityDensity")
        updateProbabilityDensitySection(input)
      } else if (slb_type %in% c("numv", "fnrv"))
        shinyjs::show("divItemNumvFnrv")
      else if (slb_type == "data")
        shinyjs::show("divItemData")
      else if (slb_type == "rsrv")
        shinyjs::show("divItemRsrv")
      else if(slb_type %in% c("param", "nonparam"))
        shinyjs::show("divItemBsrv")
    })
 
    observeEvent(input$slbProbabilityDensity,
                 updateProbabilityDensitySection(input))

    observeEvent(input$slbData, {
      updateSelectInput(session, 
                        "slbContinuousFunctions", 
                        choices  = c("Normal"), 
                        selected = "Normal")

      if (input$slbData == "")
        return(NULL)

      fit     <- fitData(dataId             = input$slbData, 
                         continuousFunction = "Normal")
      column  <- split(fit$resultMatrix, 
                       rep(seq_len(ncol(fit$resultMatrix)), 
                           each = nrow(fit$resultMatrix)))[[1]]

      updateSelectInput(session, 
                        "slbContinuousFunctions", 
                        choices = column[2:length(column)])
    })

    observeEvent(input$slbContinuousFunctions, {
      fitData(dataId             = input$slbData,
              continuousFunction = input$slbContinuousFunctions)
    })

    observeEvent(input$btnItemAdd, {
      cat("observeEvent -> input$btnItemAdd\n")
      hideAllFeedback()

      if (!validateItemModalAdd(input))
        return(NULL)

      nextRow <- length(session$userData$model()@items@items) + 1

      valide <- tryCatch(expr  = {setItem(input, nextRow)
                                  setItemScore(input, nextRow, TRUE)},
                         error = function(e) return(e))
      print(valide)

      if (inherits(valide, "simpleError")) {
        errorValue <- strsplit(valide$message, "|", fixed = TRUE)

        showFeedbackDanger(inputId = errorValue[[1]][2],
                           text    = errorValue[[1]][1],
                           session = session)
      }
    })

    observeEvent(input$btnModalEdit, {
      scoringValues <- session$userData$model()@scoring@values
      rowNum        <- parseActionButton(input$btnModalEdit)
      item          <- session$userData$model()@items@items[[rowNum]]
      itemExt       <- session$userData$itemsExt()@items[[rowNum]]
      showModal(itemModalEdit(item    = item, 
                              itemExt = itemExt, 
                              rowNum  = rowNum))
    })

    observeEvent(input$btnItemEdit, {
      
      if (!validateItemModalEdit(input))
        return(NULL)
      
      itemRow <- strtoi(input$txbRowNum)
      valide  <- tryCatch(expr  = {setItem(input, itemRow)},
                          error = function(e) return(e))
      
      if (inherits(valide, "simpleError")) {
        errorValue <- strsplit(valide$message, "|", fixed = TRUE)
        showFeedbackDanger(inputId = errorValue[[1]][2],
                           text    = errorValue[[1]][1],
                           session = session)
      } 

    }) 

    observeEvent(input$btnScoring, {
      model         <- session$userData$model()
      scoringValues <- model@scoring@values
      scoringItems  <- model@scoring@scoring
      rowNum        <- parseActionButton(input$btnScoring)
      item          <- model@items@items[[rowNum]]
      
      showModal(
        itemModalScoringEdit(
          item             = item,
          rowNum           = rowNum,
          scoringItems     = scoringItems,
          possibleScorings = getScoringMeaningsDetailed()[1:tail(scoringValues, 
                                                                 n = 1)])) # ?
    })

    observeEvent(input$btnConfirmEditScoringAllocation, {
      setItemScore(input, strtoi(input$txbRowNum), FALSE)
    })

    observeEvent(input$btnShow, {
      rowNum  <- parseActionButton(input$btnShow)
      item    <- session$userData$model()@items@items[[rowNum]]

      shinyAlert(title   = item@name,
                 message = buildShowText(item),
                 html    = TRUE,
                 classes = "icon-none")
    })

    observeEvent(input$btnDelete, {
      rowNum <- parseActionButton(input$btnDelete)
      
      model    <- session$userData$model()
      itemsExt <- session$userData$itemsExt()

      model@items@items <- model@items@items[-rowNum]
      itemsExt@items    <- itemsExt@items[-rowNum]

      session$userData$model(model)
      session$userData$itemsExt(itemsExt)

      shinyAlert(title   = "Item deleted",
                 message = paste("Item successfully deleted.",
                                 "After deleting some items CHECK",
                                 "the definition, full and relax commands", 
                                 "of remaining items!"),
                 type    = "warning")
    })

    output$tblItems <- DT::renderDT({
      model <- session$userData$model()

      if (length(model@items@items) == 0)
        return(NULL)

      addActionButtonsColumn(data.frame(matrix(unlist(toList(model@items)),
                                               nrow = length(model@items@items),
                                               byrow = TRUE)),
                             c("Name", "Type", "Definition", 
                               "Dependent item", "Actions"),
                             c("scoring", "show", "edit", "delete"),
                             ns)
    })

    output$labResultMatrix <- renderTable(
      matrixAddSpan(session$userData$resultMatrix()),
      sanitize.text.function = function(x) x
    )

    output$pltEmpTheor <- renderPlot({
      data2fit  <- session$userData$pltEmpTheor()$data2fit
      d         <- session$userData$pltEmpTheor()$d
      x         <- session$userData$pltEmpTheor()$x
      
      graphics::hist(data2fit,
                     main        = "Emp. and theor. distributions",
                     xlab        = "data", 
                     ylab        = "density",
                     probability = TRUE,
                     cex.main    = 1,
                     ylim        = c(0, max(d, graphics::hist(data2fit, 
                                                        plot = FALSE)$density)))
      graphics::lines(x, d, lwd = 2, col = "red")
    })

    output$pltQq <- renderPlot({
      data2fit <- session$userData$pltQq()$data2fit
      y        <- session$userData$pltQq()$y
      
      stats::qqplot(y, data2fit,
                    main     = "QQ-plot",
                    xlab     = "theoretical quantiles", 
                    ylab     = "sample quantiles",
                    cex.main = 1, 
                    pch      = 20)
      graphics::abline(0, 1, col = "red", lwd = 2)
    })

    output$pltCdf2 <- renderPlot({
      data2fit  <- session$userData$pltCdf2()$data2fit
      x         <- session$userData$pltCdf2()$x
      pp        <- session$userData$pltCdf2()$pp
      
      graphics::plot(stats::ecdf(data2fit),
                     main     = "Empirical and theoretical CDFs",
                     xlab     = "data",
                     ylab     = "CDF",
                     cex.main = 1,
                     pch      = 20)
      graphics::lines(x, pp, lwd = 2, col = "red")
    })

    output$pltPp <- renderPlot({
      data2fit  <- session$userData$pltPp()$data2fit
      p         <- session$userData$pltPp()$p
      
      graphics::plot(sort(p),
                     stats::ecdf(sort(data2fit))(sort(data2fit)),
                     main     = "PP-plot",
                     xlab     = "theoretical probabilities",
                     ylab     = "sample probabilities",
                     cex.main = 1,
                     pch      = 20)
      graphics::abline(0, 1, col = "red", lwd = 2)
    })
  } # end of module function

  moduleServer(id, module)
}
