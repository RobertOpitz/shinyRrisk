#' @name addActionButtons
#' @title addActionButtons
#' @description Adds an action button to table row.
#' @param id Row index.
#' @param actions Buttons to be displayed within the actions column. Possible values: scoring, edit, delete.
#' @param ns Namespace.
#' @return HTML source code.
#' @export
buildActionButtons <- function(id, actions, ns) {
  cat("buildActionButtons\n")

  tags <- tagList()

  if ('scoring' %in% actions) {
    tags <- tagAppendChild(tags,
                           actionButton(inputId = ns(paste("btnScoring", 
                                                           id, sep = "_")),
                                        label   = NULL,
                                        icon    = icon('balance-scale-left'),
                                        onclick = paste0("Shiny.setInputValue('", 
                                                         ns("btnScoring"), 
                                                         "', this.id, {priority: 'event'})")))       
  }

  if ('show' %in% actions) {
    tags <- tagAppendChild(tags,
                           actionButton(inputId = ns(paste("btnShow", 
                                                           id, sep = "_")),
                                        label   = NULL,
                                        icon    = icon('eye'),
                                        onclick = paste0("Shiny.setInputValue('", 
                                                        ns("btnShow"), 
                                                        "', this.id, {priority: 'event'})")))       
  }

  if ('edit' %in% actions) {
    tags <- tagAppendChild(tags,
                           actionButton(inputId = ns(paste("btnModalEdit", 
                                                           id, sep = "_")),
                                        label   = NULL,
                                        icon    = icon('edit'),
                                        onclick = paste0("Shiny.setInputValue('", 
                                                         ns("btnModalEdit"), 
                                                         "', this.id, {priority: 'event'})")))
  }
    
  if ('delete' %in% actions) {
    tags <- tagAppendChild(tags, 
                           actionButton(inputId = ns(paste("btnDelete", 
                                                           id, sep = "_")),
                                        label   = NULL,
                                        icon    = icon('trash'),
                                        onclick = paste0("confirm('Delete this row?') ? Shiny.setInputValue('",
                                                         ns("btnDelete"), 
                                                         "', this.id, {priority: 'event'}) : false;")))
  }

  as.character(tags)
}
