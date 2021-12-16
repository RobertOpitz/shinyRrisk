#' @name itemModalAdd
#' @title itemModalAdd
#' @description Opens dialogue to add new item.
#' @param id Namespace identifier.
#' @return HTML source code.
#' @export
itemModalAdd <- function(id = "items") {
  cat("itemModalAdd\n")
  ns <- NS(id)
  
  modalDialog(title     = "Add variable to model",
              size      = "l",
              tabsetPanel(id   = ns("tabItem"),
                          type = "tabs",
                          itemTabDefinition(),
                          itemTabFit(),
                          hr(),
                          strong("* required information")),
              easyClose = FALSE,
              footer    = tagList(modalButton(label = "Cancel", 
                                              icon  = icon("times-circle")),
                                  actionButton(inputId = ns("btnItemAdd"),
                                               label   = "Save",
                                               icon    = icon("plus"))))
}
