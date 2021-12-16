#' @name uiUncertainties
#' @title uiUncertainties
#' @description Builds the UI of the uncertainties context.
#' @param id Namespace identifier.
#' @return HTML source code.
#' @export
uiUncertainties <- function(id = "uncertainties") {
  cat("uiUncertainties\n")
  ns <- NS(id)
  
  tabItem(tabName = "uncertainties",
          fluidRow(box(title       = "Modeling approach uncertainties",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       shinyjs::hidden(numericInput(inputId = ns("nuiModellingUncertY"), 
                                                    label   = "", 
                                                    value   = 1)),
                       shinyjs::hidden(plotOutput(outputId = ns('pltUncertaintiesModellingUncert'),
                                                  height   = "auto")),
                       br(),
                       DT::dataTableOutput(outputId = ns("tblModelUncertCategories")))),
          fluidRow(box(title       = "Scenario uncertainties",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       shinyjs::hidden(numericInput(inputId = ns("nuiScenarioUncertY"), 
                                                    label   = "", 
                                                    value   = 1)),
                       shinyjs::hidden(plotOutput(outputId = ns('pltUncertaintiesScenarioUncert'),
                                                  height   = "auto")),
                       br(),
                       DT::dataTableOutput(outputId = ns("tblSceneUncertCategories")))),
          fluidRow(box(title       = "Other uncertainties",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       shinyjs::hidden(numericInput(inputId = ns("nuiOtherUncertY"), 
                                                    label   = "", 
                                                    value   = 1)),
                       shinyjs::hidden(plotOutput(outputId = ns('pltUncertaintiesOtherUncert'),
                                                  height   = "auto")),
                       br(),
                       DT::dataTableOutput(outputId = ns("tblOtherUncertCategories")))),
          fluidRow(box(title       = "Add Category",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       actionButton(inputId = ns("btnOpenModal"), 
                                    label   = "Add Category"),
                       actionButton(inputId = ns("btnReset"), 
                                    label   = "Reset System"))))
}
