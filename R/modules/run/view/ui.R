#' @name uiRun
#' @title uiRun
#' @description Builds the UI of the run context.
#' @param id Namespace identifier.
#' @return HTML source code.
#' @export
uiRun <- function(id = "run") {
  cat("uiRun\n")
  ns <- NS(id)
  
  tabItem(tabName = "run",
          fluidRow(box(title       = "Run",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       radioButtons(inputId  = ns("rabRun"),
                                    label    = "Select a run mode:",
                                    choices  = list("Model (1d)" = "1d",
                                                    "Model (1d + 2d)" = "2d"),
                                    selected = "1d",
                                    inline   = FALSE),
                       hidden(selectInput(inputId  = ns("slbItems"), 
                                          label    = "Pick OF item", 
                                          choices  = c(""), 
                                          selected = NULL, 
                                          multiple = FALSE)),
                       hr(),
                       actionButton(inputId = ns("btnRun"), 
                                    label   = "Run Simulation"))),
          fluidRow(box(title       = "Simulation settings",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       numericInput(inputId = ns("txbN"), 
                                    label   = paste("Number of iterations",
                                                    "(1st dimension)"), 
                                    value   = 10000),
                       numericInput(inputId = ns("txbN2d"), 
                                    label   = paste("Number of iterations", 
                                                    "(2st dimension)"), 
                                    value   = 50),
                       numericInput(inputId = ns("txbAbserrors"), 
                                    label   = paste("Absolute error tolerance", 
                                                    "relevant for",
                                                    "plotting convergence"), 
                                    value   = 0.0001),
                       actionButton(inputId = ns("btnSave"), 
                                    label   = "Save Settings"))),
          #fluidRow(box(title       = "Save simulation settings",
          #             solidHeader = TRUE,
          #             status      = "primary",
          #             width       = 12,
          #             actionButton(inputId = ns("btnSave"), 
          #                          label   = "Save"))),
          fluidRow(box(title       = "Reset results",
                       solidHeader = TRUE,
                       status      = "primary",
                       width       = 12,
                       actionButton(inputId = ns("btnReset"), 
                                    label   = "Reset"))))
}
