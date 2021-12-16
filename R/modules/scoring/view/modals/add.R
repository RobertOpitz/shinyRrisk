#' @name scoringModalAdd
#' @title scoringModalAdd
#' @description Opens dialogue to add new scoring item.
#' @param id Namespace identifier.
#' @return
#' @export
scoringModalAdd <- function(id = "scoring") {
  cat("scoringModalAdd\n")
  ns <- NS(id)
  
  modalDialog(title = "Add criteria to model",
              tagList(textInput(inputId     = ns("txbNotation"), 
                                label       = "Symbol *", 
                                value       = "", 
                                placeholder = ""),
                      textInput(inputId     = ns("txbName"), 
                                label       = "Name *", 
                                value       = "", 
                                placeholder = ""),
                      textAreaInput(inputId     = ns("tbaExplanation"), 
                                    label       = "Explanation *", 
                                    value       = "", 
                                    placeholder = ""),
                      hr(),
                      strong("* required information")),
              easyClose = FALSE,
              footer = tagList(modalButton(label = "Cancel", 
                                           icon  = icon("times-circle")),
                               actionButton(inputId = ns("btnAdd"), 
                                            label   = "Save", 
                                            icon    = icon("plus"))))
}