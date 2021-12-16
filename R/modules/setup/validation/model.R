#' @name validateSetupInput
#' @title validateSetupInput
#' @description Validates setup context and provides direct feedback to user.
#' @param input Contains UI input objects.
#' @return Boolean value (valid / invalid).
#' @export
validateSetupInput <- function(input) {
  cat("validateSetupInput\n")
  checkRules(inputId = "txbModelName", 
             value   = input$txbModelName, 
             rules   = list("required", list("limit", 200)))
}
