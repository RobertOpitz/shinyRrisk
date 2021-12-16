#' @name uncertaintyModalEdit
#' @title uncertaintyModalEdit
#' @description Opens dialogue to edit existing uncertainty item.
#' @param id Namespace identifier.
#' @param scoringItems Scoring items defined in scoring system.
#' @param possibleScorings Scoring values defined in scoring system.
#' @param uncertaintyScore Uncertainty score to be edited.
#' @param rowNum List index.
#' @return
#' @export
uncertaintyModalEdit <- function(id = "uncertainties", 
                                 scoringItems, 
                                 possibleScorings, 
                                 uncertaintyScore, rowNum) {
  cat("uncertaintyModalEdit\n")
  ns <- NS(id)
  
  modalDialog(title = "Edit category",
              tagList(hidden(textInput(inputId = ns("txbRowNum"), 
                                       label   = "", 
                                       value   = rowNum)),
                      selectInput(inputId  = ns("slbMaincategory"), 
                                  label    = "Assessment of *", 
                                  choices  = names(getUncertaintyTypes()), 
                                  selected = uncertaintyScore@namemain),
                      textInput(inputId     = ns("txbNameSub"), 
                                label       = "Sub category *", 
                                value       = uncertaintyScore@namesub, 
                                placeholder = ""),
                      textAreaInput(inputId     = ns("tbaExplanation"), 
                                    label       = "Explanation", 
                                    value       = uncertaintyScore@explanation, 
                                    placeholder = ""),
                      {lapply(seq_along(scoringItems), 
                              function(i) {
                                 fluidRow(box(width = 6,
                                              disabled(textInput(inputId = ns(paste0("txbScoringItem", 
                                                                                     toString(i))), 
                                                                 label   = scoringItems[[i]]@notation, 
                                                                 value   = scoringItems[[i]]@name))),
                                          box(width = 6,
                                              selectInput(inputId  = ns(paste0("slbScores", 
                                                                               toString(i))), 
                                                          label    = "Score", 
                                                          choices  = possibleScorings, 
                                                          selected = uncertaintyScore@scores[[i]])))})},
                      hr(),
                      strong("* required information")),
              easyClose = FALSE,
              footer = tagList(modalButton(label = "Cancel", 
                                           icon  = icon("times-circle")),
                               actionButton(inputId = ns("btnEdit"), 
                                            label   = "Save", 
                                            icon    = icon("edit"))))
}
