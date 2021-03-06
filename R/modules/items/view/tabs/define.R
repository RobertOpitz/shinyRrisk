#' @name itemTabDefinition
#' @title itemTabDefinition
#' @description Builds the UI of the item definition tab within item add modal.
#' @param id Namespace identifier.
#' @return HTML source code.
#' @export
itemTabDefinition <- function(id = "items") {
  cat("itemTabDefinition\n")
  ns <- NS(id)
  
  tabPanel(title = "Definition",
           fluidRow(box(width = 6,
                        hidden(textInput(inputId = ns("txbRowNum"), 
                                         label   = "", 
                                         value   = "0")),
                        textInput(inputId     = ns("txbName"), 
                                  label       = "Name *", 
                                  value       = "", 
                                  placeholder = ""),
                        # rename title to description (frontend only)
                        textInput(inputId     = ns("txbTitle"), 
                                  label       = "Description", 
                                  value       = "", 
                                  placeholder = ""),
                        selectInput(inputId  = ns("slbRole"), 
                                    label    = "Role *", 
                                    choices  = getItemRoles(), 
                                    selected = getItemRoles()[1]),
                        selectInput(inputId  = ns("slbType"), 
                                    label    = tags$span("Type *", 
                                                         tags$i(class = "fa fa-info info-title", 
                                                                title = getItemToolTips(field = "type"))), 
                                    choices  = getItemTypes(),
                                    selected = "")),
                    box(width = 6,
                        textInput(inputId     = ns("txbUnit"), 
                                  label       = tags$span("Unit", 
                                                           tags$i(class = "fa fa-info info-title", 
                                                                  title = getItemToolTips(field = "unit"))), 
                                  value       = "", 
                                  placeholder = ""),
                        textInput(inputId     = ns("txbAssumptions"), 
                                  label       = tags$span("Assumptions", 
                                                          tags$i(class = "fa fa-info info-title", 
                                                                 title = getItemToolTips(field = "assumptions"))), 
                                  value       = "", 
                                  placeholder = ""),
                        textInput(inputId     = ns("txbReference"), 
                                  label       = tags$span("References", 
                                                          tags$i(class = "fa fa-info info-title", 
                                                                 title = getItemToolTips(field = "reference"))), 
                                  value       = "", 
                                  placeholder = ""))),
           shinyjs::hidden(fluidRow(id = ns("divItemProbabilityDensity"),
                                    box(width = 6,
                                        selectInput(inputId  = ns("slbProbabilityDensity"), 
                                                    label    = "Probability Density *", 
                                                    choices  = getProbabilityDensity(), 
                                                    selected = ""),
                                        ),
                                    box(width = 6,
                                        numericInput(inputId = ns("txbPlausibleMin"), 
                                                     label   = "Plausible min *", 
                                                     value   = 0),
                                        numericInput(inputId = ns("txbPlausibleMax"), 
                                                     label   = "Plausible max *", 
                                                     value   = 0.2)))),
           shinyjs::hidden(fluidRow(id = ns("divItemMcrv"),
                                    box(width = 6,
                                        shinyjs::hidden(textInput(inputId     = ns("txbValue1"), 
                                                                  label       = "Value 1 *", 
                                                                  value       = "", 
                                                                  placeholder = "")),
                                        shinyjs::hidden(textInput(inputId     = ns("txbValue3"), 
                                                                  label       = "Value 3 *", 
                                                                  value       = "", 
                                                                  placeholder = ""))),
                                    box(width = 6,
                                        shinyjs::hidden(textInput(inputId     = ns("txbValue2"), 
                                                                  label       = "Value 2 *", 
                                                                  value       = "", 
                                                                  placeholder = "")),
                                        shinyjs::hidden(textInput(inputId     = ns("txbValue4"), 
                                                                  label       = "Value 4 *", 
                                                                  value       = "", 
                                                                  placeholder = ""))))),
           shinyjs::hidden(fluidRow(id = ns("divItemDiscrete"),
                                    box(width = 12,
                                        fileInput(inputId     = ns("filDiscrete"), 
                                                  label       = "Discrete *", 
                                                  multiple    = FALSE, 
                                                  accept      = c(".csv"), 
                                                  buttonLabel = "Browse...", 
                                                  placeholder = "Select a CSV file")))),
           shinyjs::hidden(fluidRow(id = ns("divItemNumvFnrv"),
                                    box(width = 12,
                                        textInput(inputId     = ns("txbNumvFnrv"), 
                                                  label       = "Definition *", 
                                                  value       = "", 
                                                  placeholder = "")))),
           shinyjs::hidden(fluidRow(id = ns("divItemData"),
                                    box(width = 12,
                                        fileInput(inputId     = ns("filDataFile"), 
                                                  label       = "Data *", 
                                                  multiple    = FALSE, 
                                                  accept      = c(".csv"), 
                                                  buttonLabel = "Browse...", 
                                                  placeholder = "Select a CSV file")))),
           shinyjs::hidden(fluidRow(id = ns("divItemRsrv"),
                                    box(width = 12,
                                        selectInput(inputId  = ns("slbRsrv"), 
                                                    label    = "Choose data set for resampling *", 
                                                    choices  = getItemSet(includeMcrvFnrv = TRUE), 
                                                    selected = "")))),
           shinyjs::hidden(fluidRow(id = ns("divItemBsrv"),
                                    box(width = 12,
                                        selectInput(inputId  = ns("slbBsrv"), 
                                                    label    = "Choose data for bootstrapping *", 
                                                    choices  = getItemSet(includeMcrvFnrv = FALSE), 
                                                    selected = "")))),
           icon = icon("edit"))
}
