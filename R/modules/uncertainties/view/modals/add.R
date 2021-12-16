#' @name uncertaintyModalAdd
#' @title uncertaintyModalAdd
#' @description Opens dialogue to add new uncertainty item.
#' @param id Namespace identifier.
#' @param scoringItems Scoring items defined in scoring system.
#' @param possibleScorings Scoring values defined in scoring system.
#' @return
#' @export
uncertaintyModalAdd <- function(id = "uncertainties", 
                                scoringItems, possibleScorings) {
  cat("uncertaintyModalAdd\n")
  
  ns <- NS(id)
  
  modalDialog(title = "Add category",
              tagList(selectInput(inputId  = ns("slbMaincategory"), 
                                  label    = "Assessment of *", 
                                  choices  = names(getUncertaintyTypes()), 
                                  selected = getUncertaintyTypes()[1]),
                      textInput(inputId     = ns("txbNameSub"), 
                                label       = "Sub category *", 
                                value       = "", 
                                placeholder = ""),
                      textAreaInput(inputId     = ns("tbaExplanation"), 
                                    label       = "Explanation", 
                                    value       = "", 
                                    placeholder = ""),
                      mapply(function(si, i) {
                        fluidRow(box(width = 6,
                                     disabled(textInput(
                                       inputId = ns(paste0("txbScoringItem",
                                                           toString(i))),
                                       label   = si@notation,
                                       value   = si@name))),
                                 box(width = 6,
                                     selectInput(inputId = ns(paste0("slbScores",
                                                                     toString(i))),
                                                 label   = "Score",
                                                 choices = possibleScorings)))
                      }, scoringItems, seq_along(scoringItems), 
                      SIMPLIFY = FALSE),
                      hr(),
                      strong("* required information")),
              easyClose = FALSE,
              footer    = tagList(modalButton(label = "Cancel", 
                                              icon  = icon("times-circle")),
                                  actionButton(inputId = ns("btnAdd"), 
                                               label   = "Save", 
                                               icon    = icon("plus"))))
}
