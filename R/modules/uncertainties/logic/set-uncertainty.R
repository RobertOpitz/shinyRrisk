#' @name setUncertainty
#' @title setUncertainty
#' @description Adds or replaces an uncertainty in the uncertainties object depending on the row number.
#' @param input Contains UI input objects.
#' @param rowNum List index to replace or add uncertainty.
#' @return NULL
#' @export
setUncertainty <- function(input, rowNum) {
  cat("setUncertainty\n")
  session <- shiny::getDefaultReactiveDomain()
  model   <- session$userData$model()

  scores <- sapply(seq_along(model@scoring@scoring),
                   function(i, input) {
                     as.numeric(input[[paste0("slbScores", toString(i))]])
                   }, input)

  model@uncertainties@uncertainties[[rowNum]] <- new(
    Class       = "uncertClass",
    namemain    = input$slbMaincategory,
    namesub     = input$txbNameSub,
    explanation = input$tbaExplanation,
    scores      = scores)

  session$userData$model(model)

  removeModal()
}