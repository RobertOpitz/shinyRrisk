#' @name displayScoringSystem
#' @title displayScoringSystem
#' @description Updates UI of uncertainties context.
#' @param output Contains UI output objects.
#' @param ns Namespace.
#' @return NULL
#' @export
displayScoringSystem <- function(output, ns) {
  cat("displayScoringSystem\n")

  session <- shiny::getDefaultReactiveDomain()
  model   <- session$userData$model()

  updateTextInput(session = session, 
                  inputId = "txbSystemName", 
                  value   = model@scoring@name)     
  updateNumericInput(session = session, 
                     inputId = "numScores", 
                     value   = tail(model@scoring@values, 
                                  n = 1))
  updateTextAreaInput(session = session, 
                      inputId = "tbaExplanatory", 
                      value   = model@scoring@explanatory)

  output$tblScores <- DT::renderDT({

    if (length(model@scoring@scoring) == 0)
      return(NULL)

    addActionButtonsColumn(data.frame(matrix(unlist(toList(model@scoring)),
                                             nrow = length(model@scoring@scoring),
                                             byrow = TRUE)),
                           c("Symbol", "Name", "Explanation", "Actions"),
                           c('edit', 'delete'),
                           ns)
  })
}
