#' @name setAuthor
#' @title setAuthor
#' @description Adds or replaces an author in the authors list.
#' @param input Contains UI input objects.
#' @param rowNum List index to replace or add author.
#' @return NULL
#' @export
setAuthor <- function(input, rowNum) {
  cat("setAuthor\n")
  model <- shiny::getDefaultReactiveDomain()$userData$model()

  author <- new(Class       = "authorClass",
                name        = input$txbName,
                institution = input$txbInstitution,
                email       = input$txbEmail)
  
  model@authors@authors[[rowNum]] <- author
  
  shiny::getDefaultReactiveDomain()$userData$model(model)

  removeModal()
}
