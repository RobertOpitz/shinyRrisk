#' @name checkRules
#' @title checkRules
#' @description Applies rules to an input object. On failure shows immediate feedback to user.
#' @param inputId Input identifier.
#' @param value Input value.
#' @param rules Rules to be applied.
#' @return Boolean
#' @export
checkRules <- function(inputId, value, rules) {
  cat("checkRules\n")

  check <- TRUE
  
  for (rule in rules) {
    
    if (is.list(rule)) {
      rule  <- unlist(rule)
      valid <- getRuleSet(rule[1])(value = value, 
                                   compare = rule[2])
    } else
      valid <- getRuleSet(rule[1])(value = value)
    
    if (valid)
      hideFeedback(inputId = inputId, 
                   session = shiny::getDefaultReactiveDomain())
    else {
      showFeedbackDanger(inputId = inputId, 
                         text    = getErrorMessage(rule[1]), 
                         session = shiny::getDefaultReactiveDomain())
      check <- FALSE
      break
    }
  }

  check
}
