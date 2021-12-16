#' @name validateScoringContext
#' @title validateScoringContext
#' @description Validates scoring context and provides direct feedback to user.
#' @param input Contains UI input objects.
#' @return Boolean value (valid / invalid).
#' @export
validateScoringContext <- function(input) {
  cat("validateScoringContext\n")
  checkRules(inputId = "txbSystemName", 
             value   = input$txbSystemName, 
             rules   = c("required")
  ) &
  checkRules(inputId = "tbaExplanatory", 
             value   = input$tbaExplanatory, 
             rules   = c("required"))
}
