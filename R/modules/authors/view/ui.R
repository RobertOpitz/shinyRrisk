#' @name uiAuthors
#' @title uiAuthors
#' @description Builds the UI of the authors context.
#' @param id Namespace identifier.
#' @return HTML source code.
#' @export
uiAuthors <- function(id = "authors") {
  cat("uiAuthors\n")
  ns <- NS(id)
  
  tabItem(tabName = "authors",
          fluidRow(box(title       = "Authors",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       DT::dataTableOutput(outputId = ns("tblAuthors")))),
          fluidRow(box(title       = "Add author",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       actionButton(inputId = ns("btnModalAdd"), 
                                    label   = "Add author", 
                                    icon    = icon("plus")))))
}
