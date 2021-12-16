#' @name itemModalEdit
#' @title itemModalEdit
#' @description Opens dialogue to edit existing item.
#' @param id Namespace identifier.
#' @param item Item to be edited.
#' @param itemExt ItemExtension to provide all item details.
#' @param rowNum List index.
#' @return HTML source code.
#' @export
itemModalEdit <- function(id = "items", item = item, itemExt, rowNum) {
  cat("itemModalEdit\n")
  ns <- NS(id)
  
  modalDialog(title = "Edit item",
              size  = "l",
              tagList(fluidRow(box(width = 6,
                                   hidden(textInput(inputId = ns("txbRowNum"), 
                                                    label   = "", 
                                                    value   = rowNum)),
                                   shinyjs::disabled(textInput(inputId     = ns("txbName"), 
                                                               label       = "Name *", 
                                                               value       = item@name, 
                                                               placeholder = "")),
                                   # rename title to description (frontend only)
                                   textInput(inputId     = ns("txbTitle"), 
                                             label       = "Description", 
                                             value       = item@title, 
                                             placeholder = ""),
                                   selectInput(inputId  = ns("slbRole"), 
                                               label    = "Role *", 
                                               choices  = getItemRoles(), 
                                               selected = getItemRoles()[item@role]),
                                   selectInput(inputId = ns("slbType"), 
                                               label = tags$span("Type *", 
                                                                 tags$i(class = "fa fa-info info-title", 
                                                                        title = getItemToolTips(field = "type"))), 
                                               choices = getItemTypes(), 
                                               selected = getItemTypes()[item@type])),
                               box(width = 6,
                                   textInput(inputId = ns("txbUnit"), 
                                             label = "Unit", 
                                             value = item@unit, 
                                             placeholder = ""),
                                   textInput(inputId = ns("txbAssumptions"), 
                                             label = tags$span("Assumptions", 
                                                               tags$i(class = "fa fa-info info-title", 
                                                                      title = getItemToolTips(field = "assumptions"))), 
                                             value = item@assumptions, 
                                             placeholder = ""),
                                   textInput(inputId = ns("txbReference"), 
                                             label = tags$span("References", 
                                                               tags$i(class = "fa fa-info info-title", 
                                                                      title = getItemToolTips(field = "reference"))), 
                                             value = item@reference, 
                                             placeholder = ""))),
                      {if (item@typecode == "mcrv") {
                        fluidRow(id = ns("divItemProbabilityDensity"),
                                 box(width = 6,
                                     {selectInput(inputId = ns("slbProbabilityDensity"), 
                                                  label = "Probability Density *", 
                                                  choices = getProbabilityDensity(), 
                                                  selected = itemExt@probabilityDensity)}),
                                 box(width = 6,
                                     numericInput(inputId = ns("txbPlausibleMin"), 
                                                  label = "Plausible min *", 
                                                  value = item@plausimin),
                                     numericInput(inputId = ns("txbPlausibleMax"), 
                                                  label = "Plausible max *", 
                                                  value = item@plausimax)))
                        } else {
                          shinyjs::hidden(fluidRow(id = ns("divItemProbabilityDensity"),
                                                   box(width = 6,
                                                       {selectInput(inputId  = ns("slbProbabilityDensity"), 
                                                                    label    = "Probability Density *", 
                                                                    choices  = getProbabilityDensity(), 
                                                                    selected = itemExt@probabilityDensity)}),
                                                   box(width = 6,
                                                       numericInput(inputId = ns("txbPlausibleMin"), 
                                                                    label   = "Plausible min *", 
                                                                    value   = item@plausimin),
                                                       numericInput(inputId = ns("txbPlausibleMax"), 
                                                                    label   = "Plausible max *", 
                                                                    value   = item@plausimax))))
                          }
                        },
                      fluidRow(id = ns("divItemMcrv"),
                               box(width = 6,
                                   {if(itemExt@probabilityDensity == "") {
                                     textInput(inputId     = ns("txbValue1"), 
                                               label       = "Definition *", 
                                               value       = itemExt@value1, 
                                               placeholder = "")
                                     } else {
                                       textInput(inputId     = ns("txbValue1"), 
                                                 label       = paste(getProbabilityDensityValues(itemExt@probabilityDensity)[1], "*"), 
                                                 value       = itemExt@value1, 
                                                 placeholder = "")
                                     }},
                                   {if (length(getProbabilityDensityValues(itemExt@probabilityDensity)) > 2) {
                                     textInput(inputId     = ns("txbValue3"), 
                                               label       = paste(getProbabilityDensityValues(itemExt@probabilityDensity)[3], "*"), 
                                               value       = itemExt@value3, 
                                               placeholder = "")
                                    } else {
                                      shinyjs::hidden(textInput(inputId     = ns("txbValue3"), 
                                                                label       = "Value 3 *", 
                                                                value       = itemExt@value3, 
                                                                placeholder = ""))
                                    }}),
                               box(width = 6,
                                   {if (length(getProbabilityDensityValues(itemExt@probabilityDensity)) > 1) {
                                     textInput(inputId     = ns("txbValue2"), 
                                               label       = paste(getProbabilityDensityValues(itemExt@probabilityDensity)[2], "*"), 
                                               value       = itemExt@value2, 
                                               placeholder = "")
                                    } else {
                                      shinyjs::hidden(textInput(inputId     = ns("txbValue2"), 
                                                                label       = "Value 2 *", 
                                                                value       = itemExt@value2, 
                                                                placeholder = ""))
                                    }},
                                   {if (length(getProbabilityDensityValues(itemExt@probabilityDensity)) > 3) {
                                     textInput(inputId     = ns("txbValue4"), 
                                               label       = paste(getProbabilityDensityValues(itemExt@probabilityDensity)[4], "*"), 
                                               value       = itemExt@value4, 
                                               placeholder = "")
                                    } else {
                                      shinyjs::hidden(textInput(inputId     = ns("txbValue4"), 
                                                                label       = "Value 4 *", 
                                                                value       = itemExt@value4, 
                                                                placeholder = ""))
                                    }})),
                      fluidRow(id = ns("divItemNumvFnrv"),
                               box(width = 12,
                                   textInput(inputId     = ns("txbNumvFnrv"), 
                                             label       = "Definition *", 
                                             value       = itemExt@definition, 
                                             placeholder = ""))),
                      hr(),
                      strong("* required information")),
              easyClose = FALSE,
              footer = tagList(modalButton(label = "Cancel", 
                                           icon  = icon("times-circle")),
                               actionButton(inputId = ns("btnItemEdit"), 
                                            label   = "Save", 
                                            icon    = icon("edit"))))
}
