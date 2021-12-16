#' @name menu.bsrv
#' @aliases menu.bsrv
#' @title Non-executable auxiliary function
#' @usage menu.bsrv(item,rriskModel,method="param",menuLevel=1)
#' @param item ...
#' @param rriskModel ...
#' @param method character string defining bootstrapping method, \code{method=c("param","nonparam")}
#' @param menuLevel ...
#' @keywords items
#' @export

menu.bsrv <- function(item, rriskModel, method = "param", 
                      menuLevel = 1, itemId) {
  cat("menu.bsrv\n")
  
  #on.exit(return(invisible(item)))
  
  #levelTabulator <- paste(rep("\t", menuLevel), sep = "", collapse = "")
  #levelTabulator <- paste("\n", levelTabulator, sep = "")
  
  # get all data sets assosiated with the current model
  availableData <- c()
  items <- rriskModel@items@items
  
  #if (length(items) > 0) {
  #  for (i in 1:length(items)) {
  for (i in seq_along(items)) {
      if (items[[i]]@typecode == "data" & !is.null(items[[i]]@data)) {
        item.name <- items[[i]]@name
        var.names <- names(items[[i]]@data)
        availableData.temp <- paste(item.name, var.names, sep = "$")
        availableData <- c(availableData, availableData.temp)
      }
    #}
  } 
  
  choices <- c(availableData, "Exit dialog")
  input <- 99
  
  # EDIT(1)
  while (!is.element(input,seq(1:length(choices)))) {
    
    # EDIT(1)
    # input <- mymenu(title = "Please choose data for bootstraping", choices = choices, help = "No further help available", levelTabulator = levelTabulator)
    input <- itemId
    
    if (input <= (length(choices) - 1)) {
      data2fitname <- availableData[as.numeric(input)]
      item <- bsrv.fittodata(data2fitname, item, rriskModel, method)
      break
    } else if (input == length(choices)) { 
      break 
    }
    
    input <- 99
    
    # EDIT(1)
  } # end while
  
  return(invisible(item))
} # end of function menu.bsrv.nonparam()