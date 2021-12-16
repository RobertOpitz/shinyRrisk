#' @name menu.mcrv.fittodata
#' @aliases menu.mcrv.fittodata
#' @title Non-executable auxiliary function
#' @usage menu.mcrv.fittodata(item,rriskModel,menuLevel=1)
#' @param item ...
#' @param rriskModel ...
#' @param menuLevel ...
#' @keywords items
#' @export

menu.mcrv.fittodata <- function(item, rriskModel, menuLevel = 1, 
                                itemId, continuousFunction) {
  cat("menu.mcrv.fittodata\n")
  
  #on.exit(return(invisible(item)))
  
  # get all data sets assosiated with the current model
  availableData.names <- c()
  availableData.data  <- list()
  items               <- rriskModel@items@items
  
  #if (length(items) > 0) {
  #  for (i in 1:length(items)) {
  #for (i in seq_along(items)) {
  for (this_item in items) {
      # Betrachte data-Items, bei denen data-Slot nicht leer ist
      if (this_item@typecode == "data" && !is.null(this_item@data)) {
        item.name <- this_item@name
        var.names <- names(this_item@data)
        availableData.temp  <- paste(item.name, var.names, sep = "$")
        availableData.names <- c(availableData.names, availableData.temp)
        availableData.data  <- c(availableData.data, as.list(this_item@data))
      }
    #}
  }
  
  choices <- c(availableData.names, "Exit dialog")
  cat("menu.mcrv.fittodata -> choices =", choices, "\n")
  input <- 99
  
  while (!is.element(input, seq(1:length(choices)))) { # ?
    
    input <- itemId # ?
    
    if (input == length(choices))
      break
    else if (input <= (length(choices) - 1)) {
      data2fitname <- availableData.names[as.numeric(input)]
      data2fit     <- availableData.data[[as.numeric(input)]]
      
      # wandele in ein vektor um
      data2fit <- as.vector(unlist(data2fit))
      cat(data2fitname, ": ", head(data2fit), "...\n")
      
      item <- mcrv.fittodata(data2fit, 
                             data2fitname, 
                             item, 
                             rriskModel, 
                             continuousFunction = continuousFunction)
      break
    }
    
    input <- 99
  }
  
  item
}