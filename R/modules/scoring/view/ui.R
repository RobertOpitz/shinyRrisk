#' @name uiScoring
#' @title uiScoring
#' @description Builds the UI of the scoring system context.
#' @param id Namespace identifier.
#' @return HTML source code.
#' @export
uiScoring <- function(id = "scoring") {
  cat("uiScoring\n")
  ns <- NS(id)

  tabItem(tabName = "scoring",
          fluidRow(box(title       = "Scoring System",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       textInput(inputId     = ns("txbSystemName"), 
                                 label       = "Scoring System Name *", 
                                 value       = "", 
                                 placeholder = ""),
                       textAreaInput(inputId     = ns("tbaExplanatory"), 
                                     label       = "Explanation *", 
                                     value       = "", 
                                     placeholder = ""),
                       hr(),
                       strong("* required information"))),
          fluidRow(box(title       = "Scoring Values for Uncertainties",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       tableOutput(outputId = ns("tblScoresExplanation")))), 
          fluidRow(box(title       = "Dimensions (Criteria) of Uncertainty",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       DT::dataTableOutput(outputId = ns("tblScores")))),
          fluidRow(box(title       = "Add Criteria",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       actionButton(inputId = ns("btnOpenModal"), 
                                    label   = "Add Criteria"),
                       actionButton(inputId = ns("btnReset"), 
                                    label   = "Reset System"))))
}
