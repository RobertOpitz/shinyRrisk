getExportName <- function(datatype) {
  cat("getExportName\n")
  paste0("report-",
         shiny::getDefaultReactiveDomain()$userData$model()@name@name,
         "-", format(Sys.time(), "%Y-%m-%d"), ".", datatype)
}
