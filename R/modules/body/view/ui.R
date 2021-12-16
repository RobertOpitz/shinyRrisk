#' @name uiBody
#' @title uiBody
#' @description Builds the UI of the dashboard body containing all UI contexts.
#' @param id Namespace identifier.
#' @return HTML source code.
#' @export
uiBody <- function(id = "body") {
  cat("uiBody\n")
  ns <- NS(id)
  
  dashboardBody(tabItems(uiAbout(),
                         uiAnalysis(),
                         uiAuthors(),
                         uiBasics(),
                         uiIntroduction(),
                         uiItems(),
                         uiRun(),
                         uiScoring(),
                         uiSetup(),
                         uiSummary(),
                         uiUncertainties()),
                # custom css
                includeCSS("./www/css/style.css"),
                # set up Shinyjs
                useShinyjs(),
                # set up ShinyAlert
                useShinyalert(),
                # set up ShinyFeedback
                useShinyFeedback())
}
