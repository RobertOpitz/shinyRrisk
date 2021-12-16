#' @name validateUncertaintyModalEdit
#' @title validateUncertaintyModalEdit
#' @description Validates uncertainty modal for editing uncertainties and provides direct feedback to user.
#' @param input Contains UI input objects.
#' @return Boolean value (valid / invalid).
#' @export
validateUncertaintyModalEdit <- function(input) {
  cat("validateUncertaintyodalEdit\n")
  checkRules(inputId = "slbMaincategory", 
             value   = input$slbMaincategory, 
             rules   = list("required")
  ) &
  checkRules(inputId = "txbNameSub", 
             value   = input$txbNameSub, 
             rules   = list("required", 
                            "startsWithLetter", 
                            list("limit", 128))
  ) & 
  checkRules(inputId = "tbaExplanation", 
             value   = input$tbaExplanation, 
             rules   = list(list("limit", 4096))
  )
}
