#' @name loadModel
#' @title loadModel
#' @description Loads model from .rdata or .rda file.
#' @param path File path.
#' @return NULL
#' @export
loadModel <- function(path) {
  cat("loadModel\n")

  fileExtension <- tolower(tools::file_ext(path))

  if (!(fileExtension %in% c("rdata", "rda"))) {
    shinyAlert(title = paste0("Extension '", fileExtension, "' not supported"),
               type  = "error")
    return(NULL)
  }

  tryCatch(expr = {
    envLoad <- new.env()
    modelNames <- load(path, envir = envLoad)
    shiny::getDefaultReactiveDomain()$userData$model(get("rriskModel", 
                                                         envir = envLoad))

    if ('itemsExt' %in% modelNames)
      shiny::getDefaultReactiveDomain()$userData$itemsExt(get("itemsExt", 
                                                              envir = envLoad))
    else {
      shinyAlert(title = paste("File does not contain model extension.",
                               "Some features might be limited"),
                 type = "warning")

      items     <- shiny::getDefaultReactiveDomain()$userData$model()@items@items
      itemsExt  <- shiny::getDefaultReactiveDomain()$userData$itemsExt()
  
      for (i in seq_along(items))
        itemsExt@items[[i]] <- new("itemClassExt", name = items[[i]]@name)

      shiny::getDefaultReactiveDomain()$userData$itemsExt(itemsExt)
    }
      
    shinyAlert(title = paste0("Import of model '", 
                              shiny::getDefaultReactiveDomain()$userData$model()@name@name, 
                              "' completed"))
  }, 
  error = function(error) {
    shinyAlert(title = paste("Error importing model ->", 
                             "Loading default model instead!"),
               type = "error")

    shiny::getDefaultReactiveDomain()$userData$model(
      new("modelClass", 
          name = new("modelNameClass", 
          name = "NA")))}
  )
}
