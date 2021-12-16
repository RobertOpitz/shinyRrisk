#' @name hideAllFeedback
#' @title hideAllFeedback
#' @description Hides all feedback from items modal.
#' @return NULL
#' @export
hideAllFeedback <- function() {
#hideAllFeedback <- function(session) {
  cat("hideAllFeedback\n")
  session <- shiny::getDefaultReactiveDomain()

  hideFeedback(inputId = "txbName", 
               session = session)
  hideFeedback(inputId = "slbRole", 
               session = session)
  hideFeedback(inputId = "slbType", 
               session = session)
  hideFeedback(inputId = "slbProbabilityDensity", 
               session = session)
  hideFeedback(inputId = "txbPlausibleMin", 
               session = session)
  hideFeedback(inputId = "txbPlausibleMax", 
               session = session)
  hideFeedback(inputId = "filDataFile", 
               session = session)

  for (i in 1:4)
    hideFeedback(inputId = paste0("txbValue", i), 
                 session = session)
}
