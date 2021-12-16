#' @name validateAuthorModal
#' @title validateAuthorModal
#' @description Validates authors modal and provides direct feedback to user.
#' @param input Contains UI input objects.
#' @return Boolean value (valid / invalid).
#' @export
validateAuthorModal <- function(input) {
  cat("validateAuthorModal\n")
  return({
    result <- checkRules(inputId = "txbName", # check name of author
                         value = input$txbName, 
                         rules = list("required", 
                                      "startsWithLetter", 
                                      list("limit", 32))) &
              checkRules(inputId = "txbInstitution", # check instituion of author
                         value   = input$txbInstitution, 
                         rules   = list("required", list("limit", 128)))

    if (input$txbEmail != "") { # check the email of author
      result <- result & checkRules(inputId = "txbEmail", 
                                    value   = input$txbEmail, 
                                    rules   = list("isValidEmail", 
                                                   list("limit", 128)))
    }

    return(result)    
  })
}
