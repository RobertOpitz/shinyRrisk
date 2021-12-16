#' @name setItemScore
#' @title setItemScore
#' @description Modifies an items scoring.
#' @param input Contains UI input objects.
#' @param rowNum List index to replace or add item.
#' @param isNewItem Boolean to determine existing or new item.
#' @return NULL
#' @export
setItemScore <- function(input, rowNum, isNewItem) {
#setItemScore <- function(model, session, input, rowNum, isNewItem) {
  cat("setItemScore\n")

  session <- shiny::getDefaultReactiveDomain()  
  model   <- session$userData$model()

  if (isNewItem)
    scores <- rep(0, length(model@scoring@scoring))
  else
    scores <- sapply(seq_along(model@scoring@scoring),
                     function(i) as.numeric(input[[paste0("slbScores", 
                                                          toString(i))]]))

  model@items@items[[rowNum]]@scores <- scores
  session$userData$model(model)

  removeModal()
}
