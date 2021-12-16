#' @name bsrv.fittodata
#' @aliases bsrv.fittodata
#' @title Non-executable auxiliary function
#' @usage bsrv.fittodata(data2fitname,item,rriskModel,method)
#' @param data2fitname ...
#' @param item ...
#' @param rriskModel ...
#' @param method character string giving the method for bootstrapping, \code{method=c("param","nonparam")}
#' @keywords items
#' @export

bsrv.fittodata <- function(data2fitname, item, rriskModel, method) {
  cat("bsrv.fittodata\n")
  
  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(item))
  
  #-----------------------------------------------------------------------------
  # define some help varibles
  #-----------------------------------------------------------------------------
  if(method == "param"){
    definition <- paste("mean, sd (param. bootstrap); fitted to ", data2fitname, sep = "")
    fullc <- paste("bootdist(fitdist(", data2fitname, ",'norm',method='mle'),bootmethod='param',niter=rriskModel@settings@N)$estim", sep = "")
  } else if (method == "nonparam") {
    definition <- paste("mean, sd (nonparam. bootstrap); fitted to ", data2fitname, sep = "")
    fullc <- paste("bootdist(fitdist(", data2fitname, ",'norm',method='mle'),bootmethod='nonparam',niter=rriskModel@settings@N)$estim", sep = "")
  }
  
  #-----------------------------------------------------------------------------
  # hier wird die Ausführbarkeit von 'fullc' geprüft
  #-----------------------------------------------------------------------------
  item.temp <- item
  item.temp@fullc <- fullc
  
  # hier wird die Ausführbarkeit von fullc geprüft
  fullcValues <- itemsEvaluation(item.temp, rriskModel)
  cat("\n***************************************\n")
  cat("Full command for",item@name)
  cat("\n***************************************\n")
  
  if (inherits(fullcValues, "try-error")) {
    cat("Full command expression", fullc, "could not be evaluated !\n")
  } else {
    # Weise Item-Slots 'definition' und 'fullc' Inhalte zu
    item@fullc <- fullc
    
    # Falls 'fullc' ausführbar ist, lasse die Zwischenergebnisse anzeigen
    cat("\nDimension of test evaluation: ", dim(fullcValues), "\n")
    cat("\nitem@fullc=", fullc, "\n")
    cat("\nGenerated item values (full command):\n"); print(head(data.frame(fullcValues))); cat("...\n")
    cat("\nSummary statistics of item values (full command):\n"); print(summary(fullcValues), digits = 4); cat("\n")
    item@fullc <- fullc
    item@data <- summary(fullcValues)
    cat("\n***************************************\nSymbolical definition for", item@name, "\n***************************************\n")
    cat("\nitem@definition=", definition, "\n")
    item@definition <- definition
    cat("\n***************************************\nRelax command for", item@name, "\n***************************************\n")
    cat("\nitem@relaxc=", fullc, "\n")
    item@relaxc <- fullc
  }
  
  #----------------------------------------------------------------------------
  # definiere Output
  #----------------------------------------------------------------------------
  return(item)
} # end of function bsrv.fittodata()