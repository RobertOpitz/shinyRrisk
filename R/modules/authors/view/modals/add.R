#' @name authorModalAdd
#' @title authorModalAdd
#' @description Opens dialogue to add new authors item.
#' @param id Namespace identifier.
#' @return HTML source code.
#' @export
authorModalAdd <- function(id = "authors") {
  cat("authorModalAdd\n")
  ns <- NS(id)
  
  modalDialog(title = "Add author to model",
              tagList(textInput(inputId     = ns("txbName"), 
                                label       = "Name *", 
                                value       = "", 
                                placeholder = ""),
                      textInput(inputId     = ns("txbInstitution"), 
                                label       = "Institution *", 
                                value       = "", 
                                placeholder = ""),
                      textInput(inputId     = ns("txbEmail"), 
                                label       = "E-Mail", 
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
