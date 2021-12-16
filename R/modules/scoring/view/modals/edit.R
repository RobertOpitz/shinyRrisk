#' @name scoringModalEdit
#' @title scoringModalEdit
#' @description Opens dialogue to edit existing scoring item.
#' @param id Namespace identifier.
#' @param scoring Scoring to be edited.
#' @param rowNum List index.
#' @return
#' @export
scoringModalEdit <- function(id = "scoring", scoring, rowNum) {
  cat("scoringModalEdit\n")
  ns <- NS(id)
  
  modalDialog(title = "Edit criteria to model",
              tagList(hidden(textInput(inputId = ns("txbRowNum"), 
                                       label   = "", 
                                       value   = rowNum)),
                      textInput(inputId     = ns("txbNotation"), 
                                label       = "Symbol *", 
                                value       = scoring@notation, 
                                placeholder = ""),
                      textInput(inputId     = ns("txbName"), 
                                label       = "Name *", 
                                value       = scoring@name, 
                                placeholder = ""),
                      textAreaInput(inputId     = ns("tbaExplanation"), 
                                    label       = "Explanation *", 
                                    value       = scoring@explanation, 
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