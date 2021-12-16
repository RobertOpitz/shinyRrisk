#' @name uiItems
#' @title uiItems
#' @description Builds the UI of the items context.
#' @param id Namespace identifier.
#' @return HTML source code.
#' @export
uiItems <- function(id = "items") {
  cat("uiItems\n")
  ns <- NS(id)

  tabItem(tabName = "items",
          fluidRow(box(title       = "Network",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       forceNetworkOutput(outputId = ns('pltD3NetworkGraph')))),
          fluidRow(box(title       = "Items",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       DT::dataTableOutput(outputId = ns("tblItems")))),
          fluidRow(box(title       = "Add item",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       actionButton(inputId = ns("btnOpenItemModal"), 
                                    label   = "Add item", 
                                    icon    = icon("plus")))))
}
