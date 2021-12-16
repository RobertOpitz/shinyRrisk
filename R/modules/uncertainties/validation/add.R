#' @name validateUncertaintyModalAdd
#' @title validateUncertaintyModalAdd
#' @description Validates uncertainty modal for adding uncertainties and provides direct feedback to user.
#' @param input Contains UI input objects.
#' @return Boolean value (valid / invalid).
#' @export
validateUncertaintyModalAdd <- function(input) {
  cat("validateUncertaintyModalAdd\n")
  checkRules(inputId = "slbMaincategory", 
             value   = input$slbMaincategory, 
             rules   = list("required")
  ) &
  checkRules(inputId = "txbNameSub", 
             value   = input$txbNameSub, 
             rules   = list("required", 
                            list("unique", getUncertaintySubCats()), 
                            "startsWithLetter", 
                            list("limit", 128))
  ) & 
  checkRules(inputId = "tbaExplanation", 
             value   = input$tbaExplanation, 
             rules   = list(list("limit", 4096))
  )
}
