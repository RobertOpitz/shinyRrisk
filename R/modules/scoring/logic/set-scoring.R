#' @name setScoring
#' @title setScoring
#' @description Adds or replaces a score in the scoring system depending on the row number.
#' @param input Contains UI input objects.
#' @param rowNum List index to replace or add score.
#' @return NULL
#' @export
setScoring <- function(input, rowNum) {
  cat("setSCoring\n")

  session <- shiny::getDefaultReactiveDomain()
  model   <- session$userData$model()
  score   <- new(Class       = "scoreClass",
                 notation    = input$txbNotation,
                 name        = input$txbName,
                 explanation = input$tbaExplanation)

  model@scoring@scoring[[rowNum]] <- score
  
  session$userData$model(model)

  removeModal()
}