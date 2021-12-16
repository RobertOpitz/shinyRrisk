#' @name uiBasics
#' @title uiBasics
#' @description Builds the UI of the basics (documentation) context.
#' @param id Namespace identifier.
#' @return HTML source code.
#' @export
uiBasics <- function(id = "basics") {
  cat("uiBasics\n")
  ns <- NS(id)
  
  tabItem(tabName = "descriptions",
          fluidRow(box(title       = "MainDoc",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       textAreaInput(inputId     = ns("txaMainDoc"), 
                                     label       = "", 
                                     value       = "", 
                                     placeholder = ""))),
          fluidRow(box(title       = "Background",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       textAreaInput(inputId     = ns("txaBackground"), 
                                     label       = "", 
                                     value       = "", 
                                     placeholder = ""))),
          fluidRow(box(title       = "Objectives",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       textAreaInput(inputId     = ns("txaObjectives"), 
                                     label       = "", 
                                     value       = "", 
                                     placeholder = ""))),
          fluidRow(box(title       = "Scope",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       textAreaInput(inputId     = ns("txaScope"), 
                                     label       = "", 
                                     value       = "", 
                                     placeholder = ""))),
          fluidRow(box(title       = "Description",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       textAreaInput(inputId     = ns("txaDescription"), 
                                     label       = "", 
                                     value       = "", 
                                     placeholder = ""))),
          fluidRow(box(title       = "Save documentation",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       actionButton(inputId = ns("btnSave"), 
                                    label   = "Save"))))
}
