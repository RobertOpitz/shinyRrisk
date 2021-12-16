#' @name serverScoring
#' @title serverScoring
#' @description Provides event handler functions.
#' @param id Namespace identifier.
#' @return NULL
#' @export
serverScoring <- function(id = "scoring") {
  cat("serverScoring\n")
  ns <- NS(id)

  module <- function(input, output, session) {

    observe(displayScoringSystem(output, ns))

    observeEvent(input$btnOpenModal, {
      showModal(scoringModalAdd())
    })

    observeEvent(input$btnAdd, {

      if (!validateScoringModal(input))
        return(NULL)

      nextRow <- length(session$userData$model()@scoring@scoring) + 1

      setScoring(input, nextRow)
    })

    observeEvent(input$txbSystemName, {

      if (!validateScoringContext(input))
        return(NULL)

      model               <- session$userData$model()
      model@scoring@name  <- input$txbSystemName

      session$userData$model(model)
    })

    output$tblScoresExplanation <- renderTable({
 
      level <- c('NA',
                 'Low', 'Low', 'Low', 'Low',
                 'Medium', 'Medium', 'Medium',
                 'High', 'High', 'High', 'High', 'High', 'High')
      symbol <- c(
        'NA',
        '0',
        '-',
        '-/+',
        '+',
        '--',
        '--/++',
        '++',
        '---',
        '---/+++',
        '+++',
        '?-',
        '?-/+',
        '?+'
      )

      explanation   <- c(
        'not applicable',
        'no discernible or a negligible effect',
        'low underestimation',
        'low underestimation or low overestimation (use this value to score C and S as Low)',
        'low overestimation',
        'moderate underestimation',
        'moderate underestimation or moderate overestimation (use this value to score C and S as Medium)',
        'moderate overestimation',
        'high underestimation',
        'high underestimation or high overestimation (use this value to score C and S as High)',
        'high overestimation',
        'underestimation of unknown magnitude',
        'underestimation or overestimation of unknown magnitude',
        'overestimation unknown magnitude'
      )

      scoreTable <- data.frame(level, symbol, explanation)

      header <- c('Level', 'Symbol', 'Explanation')
      colnames(scoreTable) <- header

      scoreTable <- format(
        scoreTable, 
        scientific = FALSE,
        striped = FALSE,
        hover = FALSE,
        bordered = FALSE,
        rownames = FALSE,
        colnames = FALSE
      )
    })

    observeEvent(input$tbaExplanatory, {

      if (!validateScoringContext(input))
        return(NULL)

      model                     <- session$userData$model()
      model@scoring@explanatory <- input$tbaExplanatory

      session$userData$model(model)
    })

    observeEvent(input$btnModalEdit, {
      rowNum  <- parseActionButton(input$btnModalEdit)
      scoring <- session$userData$model()@scoring@scoring[[rowNum]]
  
      showModal(scoringModalEdit(scoring = scoring, rowNum = rowNum))
    })

    observeEvent(input$btnEdit, {

      if (!validateScoringModal(input))
        return(NULL)
      
      setScoring(input, strtoi(input$txbRowNum))
    })

    observeEvent(input$btnDelete, {
      rowNum                <- parseActionButton(input$btnDelete)
      model                 <- session$userData$model()
      model@scoring@scoring <- model@scoring@scoring[-rowNum]

      session$userData$model(model)
    })

    observeEvent(input$btnReset, {
      model         <- session$userData$model()
      model@scoring <- initScoreSystem()

      session$userData$model(model)

      displayScoringSystem(output, ns)
    })
  }

  moduleServer(id, module)
}
