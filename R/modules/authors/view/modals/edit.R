#' @name authorModalEdit
#' @title authorModalEdit
#' @description Opens dialogue to edit existing author item.
#' @param id Namespace identifier.
#' @param author Author to be edited.
#' @param rowNum List index.
#' @return HTML source code.
#' @export
authorModalEdit <- function(id = "authors", author, rowNum) {
  cat("authorModalEdit\n")
  ns <- NS(id)
  
  modalDialog(title = "Edit author",
              tagList(hidden(textInput(inputId = ns("txbRowNum"), 
                                       label   = "", 
                                       value   = rowNum)),
                      textInput(inputId     = ns("txbName"), 
                                label       = "Name *", 
                                value       = author@name, 
                                placeholder = ""),
                      textInput(inputId     = ns("txbInstitution"), 
                                label       = "Institution *", 
                                value       = author@institution, 
                                placeholder = ""),
                      textInput(inputId     = ns("txbEmail"), 
                                label       = "E-Mail", 
                                value       = author@email, 
                                placeholder = ""),
                      hr(),
                      strong("* required information")),
              easyClose = FALSE,
              footer = tagList(modalButton(label = "Cancel", 
                                           icon  = icon("times-circle")),
                               actionButton(inputId = ns("btnEdit"), 
                                            label   = "Save", 
                                            icon    = icon("edit"))))
}
