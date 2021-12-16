#' @name itemModalScoringEdit
#' @title itemModalScoringEdit
#' @description Opens dialogue to edit scoring allocation.
#' @param id Namespace identifier.
#' @param item Item to be edited.
#' @param rowNum List index.
#' @param scoringItems Scoring items defined in scoring system.
#' @param possibleScorings Scoring values defined in scoring system.
#' @return HTML source code.
#' @export
itemModalScoringEdit <- function(id = "items", item, 
                                 rowNum, scoringItems, possibleScorings) {
  cat("itemModalScoringEdit\n")
  ns <- NS(id)
  
  # internal function
  draw_scoring_stuff <- function(i) {
    fluidRow(box(width = 6,
                 disabled(textInput(inputId = ns(paste0("txbScoringItem", i)), 
                                    label   = scoringItems[[i]]@notation, 
                                    value   = scoringItems[[i]]@name))),
             box(width = 6,
                 selectInput(inputId  = ns(paste0("slbScores", i)), 
                             label    = "Score", 
                             choices  = possibleScorings, 
                             selected = item@scores[i])))
  }
  
  modalDialog(title = paste("Edit criteria of item", item@name),
              tagList(hidden(textInput(inputId = ns("txbRowNum"), 
                                       label   = "", 
                                       value   = rowNum)),
                      {if (length(scoringItems) == 0) {
                        shinyAlert(title = "No scoring system defined",
                                   type = "error")
                        return(NULL)
                       }
                       lapply(seq_along(scoringItems), draw_scoring_stuff)
                      }),
              easyClose = FALSE,
              footer = tagList(modalButton(label = "Cancel", 
                                           icon  = icon("times-circle")),
                               actionButton(inputId = ns("btnConfirmEditScoringAllocation"), 
                                            label   = "Save", 
                                            icon    = icon("edit"))))
}
