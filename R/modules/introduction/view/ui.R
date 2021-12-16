#' @name uiIntroduction
#' @title uiIntroduction
#' @description Builds the UI of the introduction context.
#' @param id Namespace identifier.
#' @return HTML source code.
#' @export
uiIntroduction <- function(id = "introduction") {
  cat("uiIntroduction\n")
  ns <- NS(id)
  
  tabItem(tabName = "introduction",
          fluidRow(box(title = "Risk Modelling Tool",
                       solidHeader = TRUE,
                       status = "primary",
                       width = 12,
                       img(src = './img/logo.jpeg', 
                           alt = "logo", 
                           style = "float: right"),
                       p("Rrisk is a web-based tool for risk assessment using Monte-Carlo Simulation and helps to build quantitative risk models via a user-friendly shiny surface including uncertainty analysis."),
                       p("The Software is a Shiny R package consists of several modules which together compose a framework for probabilistic risk assessment."),
                       p("Features of Rrisk Shiny:"),
                       shiny::tags$ul(
                         shiny::tags$li("Model Building (Variables, Parameters, Equations, Results)"),
                         shiny::tags$li("One Dimensional and Two Dimensional Monte-Carlo Simulation"),
                         shiny::tags$li("Data Fitting"),
                         shiny::tags$li("Model Visualization"),
                         shiny::tags$li("Schema for The Assessment of Uncertainties (EFSA Guidance)"),
                         shiny::tags$li("Transparency Through Identity Between Model and its Documentation"),
                         shiny::tags$li("Auto-Reporting"),
                         shiny::tags$li("State-of-Art Risk Modelling Methodology Through Rich Functionality (Resampling, Bootstrapping, Model Network Graph)")))))
}
