#' @name uiSidebar
#' @title uiSidebar
#' @description Builds the UI of the sidebar context.
#' @param id Namespace identifier.
#' @return HTML source code.
#' @export
uiSidebar <- function(id = "sidebar") {
  cat("uiSidebar\n")
  ns <- NS(id)

  dashboardSidebar(
    sidebarMenu(menuItem(text     = "Introduction", 
                         tabName  = "introduction", 
                         selected = TRUE, 
                         icon     = icon("institution")),
                menuItem(text          = "Model",
                         tabName       = "model",
                         icon          = icon("database"),
                         selected      = TRUE,
                         startExpanded = FALSE,
                         menuSubItem(text    = "Setup", 
                                     tabName = "setup", 
                                     icon    = icon("dashboard")),
                         menuSubItem(text    = "Definitions", 
                                     tabName = "items", 
                                     icon    = icon("sitemap")),
                         menuSubItem(text    = "Uncertainties", 
                                     tabName = "uncertainties", 
                                     icon    = icon("cloud-sun-rain")),
                         menuSubItem(text    = "Authors", 
                                     tabName = "authors", 
                                     icon    = icon("users")),
                         menuSubItem(text    = "Documentation", 
                                     tabName = "descriptions", 
                                     icon    = icon("book"))),
                menuItem(text    = "Run model", 
                         tabName = "run", 
                         icon    = icon("running")),
                menuItem(text          = "Results",
                         tabName       = "results",
                         icon          = icon("area-chart"),
                         selected      = FALSE,
                         startExpanded = FALSE,
                         menuSubItem(text    = "Summary", 
                                     tabName = "summary", 
                                     icon    = icon("bar-chart")),
                         menuSubItem(text    = "Sensitivity Analysis", 
                                     tabName = "analysis", 
                                     icon    = icon("thermometer-1"))),
                menuItem(text    = "Scoring", 
                         tabName = "scoring", 
                         icon    = icon("balance-scale-left")),
                menuItem(text    = "About", 
                         tabName = "about", 
                         icon    = icon("info-circle"))),
    disable   = FALSE,
    collapsed = FALSE
  )
}
