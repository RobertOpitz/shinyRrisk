#' @name validateScoringModal
#' @title validateScoringModal
#' @description Validates scoring modal and provides direct feedback to user.
#' @param input Contains UI input objects.
#' @return Boolean value (valid / invalid).
#' @export
validateScoringModal <- function(input) {
  cat("validateScoringModal\n")
  checkRules(inputId = "txbNotation", 
             value   = input$txbNotation, 
             rules   = c("required")
  ) &
  checkRules(inputId = "txbName", 
             value   = input$txbName, 
             rules   = c("required")
  ) &
  checkRules(inputId = "tbaExplanation", 
             value   = input$tbaExplanation, 
             rules   = c("required"))
}
