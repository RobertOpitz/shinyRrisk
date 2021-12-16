#' @description Function that evaluates and saves the input numeric value
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name define.numv.fnrv
#' @aliases define.numv.fnrv
#' @title Non-executable auxiliary function
#' @usage define.numv.fnrv(item,rriskModel,menuLevel=1,type="numv")
#' @param item ...
#' @param rriskModel an instance of the class \code{modelClass}
#' @param menuLevel ...
#' @param type character string defining item type, \code{type=c("numv","fnrv")}
#' @keywords items
#' @export

define.numv.fnrv <- function(item, rriskModel, 
                             menuLevel = 1, type = "numv", value) {
  cat("define.numv.fnrv\n")
  
  # ist es ein geschichteter item?
  if (item@stratum == "")
    batch <- 1
  else {
    for (i in seq_along(rriskModel@items@items)) { # continous overwriting of variables?
      stratum.item <- rriskModel@items@items[[i]]
      if (stratum.item@name == item@stratum) {
        batch <- nrow(stratum.item@data$stratum)
        # only for fnrv items
        stid  <- as.character(stratum.item@data$stratum$stid)
        break
      }
    }
  }
  
  # Hier wird 'fullc' definiert und als Zeichenkette zurückgegeben
  myvalue.results <- myvalue(item, 
                             rriskModel = rriskModel, 
                             menuLevel  = menuLevel, 
                             batch      = batch, 
                             run        = TRUE, 
                             value      = value)
  #print(myvalue.results)
  
  # save evaluated results
  fullc  <- myvalue.results$output
  values <- myvalue.results$values
  
  # hier werden die Ergebnise angezeit
  item@fullc <- fullc # Weise Item-Slots 'definition' und 'fullc' Inhalte zu
  
  if (type == "numv" & batch > 1) {
    names(values) <- stid
  } else if (type == "fnrv") {
    if (batch == 1)
      colnames(values) <- item@name
    else
      colnames(values) <- stid
  }
  
  item@fullc      <- fullc
  item@definition <- fullc
  item@relaxc     <- fullc
  
  # save simulated item value in data-slot
  if (type == "numv")
    item@data <- values
  else if (type == "fnrv")
    item@data <- summary(values)
  
  item
}