#' @name itemTabFit
#' @title itemTabFit
#' @description Builds the UI of the data fitting tab within item add modal.
#' @param id Namespace identifier.
#' @return HTML source code.
#' @export
itemTabFit <- function(id = "items") {
  cat("itemTabFit\n")
  ns <- NS(id)

  tabPanel(title = "Fitting",
           fluidRow(box(width = 6,
                        selectInput(inputId  = ns("slbData"),
                                    label    = "Data",
                                    choices  = getItemSet(includeMcrvFnrv = FALSE),
                                    selected = "")),
                    box(width = 6,
                        selectInput(inputId = ns("slbContinuousFunctions"),
                                    label   = "Continous functions",
                                    choices = c()))),
           fluidRow(box(width = 6,
                        plotOutput(outputId = ns("pltEmpTheor"))),
                    box(width = 6,
                        plotOutput(outputId = ns("pltQq")), # ?
                        )),
           fluidRow(box(width = 6,
                        plotOutput(outputId = ns("pltCdf2"))),
                    box(width = 6,
                        plotOutput(outputId = ns("pltPp")))),
           fluidRow(box(width = 12,
                        tableOutput(outputId = ns("labResultMatrix")))),
           icon = icon("arrows-alt-h"))
}
