#' @name validateItemModalEdit
#' @title validateItemModalEdit
#' @description Validates items modal and provides direct feedback to user.
#' @param input Contains UI input objects.
#' @return Boolean value (valid / invalid).
#' @export
validateItemModalEdit <- function(input) {
  cat("validateItemModalEdit\n")
  result <- checkRules(inputId = "txbName", 
                       value = input$txbName, 
                       rules = list("required", 
                                    "startsWithLetter", 
                                    list("limit", 128))) &
            checkRules(inputId = "txbTitle", 
                       value = input$txbTitle, 
                       rules = list(list("limit", 1024))) &
            checkRules(inputId = "slbRole", 
                       value = input$slbRole, 
                       rules = list("required")) &
            checkRules(inputId = "slbType", 
                       value = input$slbType, 
                       rules = list("required")) &
            checkRules(inputId = "txbUnit", 
                       value = input$txbUnit, 
                       rules = list(list("limit", 1024))) &
            checkRules(inputId = "txbAssumptions", 
                       value = input$txbAssumptions, 
                       rules = list(list("limit", 1024))) &
            checkRules(inputId = "txbReference", 
                       value = input$txbReference, 
                       rules = list(list("limit", 1024)))

  if (input$slbType == "mcrv") {
    result <- result &
              checkRules(inputId = "slbProbabilityDensity", 
                         value = input$slbProbabilityDensity, 
                         rules = list("required")) &
              checkRules(inputId = "txbPlausibleMin", 
                         value = input$txbPlausibleMin, 
                         rules = list("required", list("limit", 16))) &
              checkRules(inputId = "txbPlausibleMax", 
                         value = input$txbPlausibleMax, 
                         rules = list("required", list("limit", 16)))
  }
  
  if (input$slbType %in% c("numv", "fnrv")) {
    result <- result &
              checkRules(inputId = "txbNumvFnrv",
                         value   = input$txbNumvFnrv,
                         rules   = list("isValidCode"))
  }
  
  result
}
