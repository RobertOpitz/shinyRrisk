#' @name buildCsv
#' @title buildCsv
#' @description Builds all result CSV files for the model.
#' @return List of export CSV files.
#' @export
buildCsv <- function() {
  cat("buildCsv\n")
  model <- shiny::getDefaultReactiveDomain()$userData$model()
  
  csv <- list(documentation = file.path(tempdir(), "documentation.csv"),
              authors       = file.path(tempdir(), "authors.csv"),
              items         = file.path(tempdir(), "items.csv"),
              summary       = file.path(tempdir(), "summary.csv"))

  write.csv(x         = buildDfDocumentation(model@basics@basics), 
            file      = csv$documentation, 
            row.names = TRUE)
  write.csv(x         = buildDfAuthors(model@authors@authors), 
            file      = csv$authors, 
            row.names = TRUE)
  write.csv(x         = buildDfItems(model@items@items), 
            file      = csv$items, 
            row.names = TRUE)
  write.csv(x         = model@output@summaries, 
            file      = csv$summary, 
            row.names = TRUE)

  csv
}
