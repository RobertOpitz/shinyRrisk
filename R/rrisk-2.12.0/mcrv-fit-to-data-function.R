#' @name mcrv.fittodata
#' @aliases mcrv.fittodata
#' @title Non-executable auxiliary function
#' @usage mcrv.fittodata(data2fit,data2fitname,item,rriskModel)
#' @param data2fit ...
#' @param data2fitname ...
#' @param item ...
#' @param rriskModel ...
#' @keywords items
#' @export

mcrv.fittodata <- function(data2fit, data2fitname, item, 
                           rriskModel, continuousFunction) {
  cat("mcrv.fittodata\n")
  
  # define output
  #on.exit(return(item))
  
  # ist ein geschichteter Item?
  batch <- 1
  if (item@stratum != ""){
    #for (i in seq_along(rriskModel@items@items)) {
    for (this_item in rriskModel@items@items) {
        if (this_item@typecode == "stra") {
          batch <- nrow(this_item@data$stratum)
          stid  <- as.character(this_item@data$stratum$stid)                                  
        } # end if
    } # end for
  } # end if
  
  # fit several distributions to data
  fitResults <- fit.cont(data2fit, continuousFunction = continuousFunction)
  
  if (!is.na(fitResults$chosenDistr)) {
    
    # gefittete Parameter
    paramNames <- names(fitResults$fittedParams)
    
    # Parameter-Infos für 'definition' and 'fullc' Slots
    #textParams <- c()
    #for (i in 1:length(paramNames)){
    #  textParams.temp <- paste0(paramNames[i], "=", 
    #                            round(fitResults$fittedParams[i], digits = 3))
    #  textParams <- c(textParams,textParams.temp)
    #}
    textParams <- sapply(seq_along(paramNames),
                         function(i) {
                           paste0(paramNames[i], "=", 
                                  round(fitResults$fittedParams[i], digits = 3))
                         })
    
    textParams  <- paste(textParams, collapse = ",")
    cat("mcrv.fittodata -> textParams =", textParams, "\n")
    chosenDistr <- fitResults$chosenDistr  # geschätze Verteilungsfamilie
    
    # aktualisiere Item Slots
    definition <- paste0(chosenDistr, "(", textParams, ");", 
                         "fitted to ", data2fitname)
    fullc      <- paste0("matrix(ncol=", batch, ",data=", "r", chosenDistr, 
                         "(n=", batch, "*rriskModel@settings@N,", 
                         textParams, "),byrow=TRUE)")
    
    # hier wird die Ausführbarkeit von 'fullc' geprüft
    item.temp       <- item
    item.temp@fullc <- fullc
    # hier wird die Ausführbarkeit von fullc geprüft
    fullcValues     <- itemsEvaluation(item.temp, rriskModel)
    
    #cat("\n***************************************\n") 
    #cat("Full command for ", item@name) 
    #cat("\n***************************************\n")
    
    if (inherits(fullcValues, "try-error")) {
      #cat("Full command expression", fullc, "could not be evaluated !\n")
    } else {
      # Weise Item-Slots 'definition' und 'fullc' Inhalte zu
      #item@fullc <- fullc
      fullcValues <- data.frame(fullcValues)
      names(fullcValues) <- item@name
      # Falls 'fullc' ausführbar ist, lasse die Zwischenergebnisse anzeigen
      #cat("\nDimension of test evaluation: ", dim(fullcValues), "\n")
      #cat("\nitem@fullc=", fullc, "\n")
      #cat("\nGenerated item values (full command):\n")
      #print(head(fullcValues))
      #cat("...\n")
      #cat("\nSummary statistics of item values (full command):\n")
      #print(summary(fullcValues), digits = 4)
      #cat("\n")
      item@fullc <- fullc
      #cat("\n***************************************\n") 
      #cat("Symbolical definition for", item@name) 
      #cat("\n***************************************\n")
      #cat("\nitem@definition=", definition, "\n")
      item@definition <- definition
      item@data <- summary(fullcValues)
    } # end if
    
    # hier wird die Ausführbarkeit von 'relaxc' geprüft
    relaxc <- paste0("matrix(ncol=", batch, 
                     ",data=runif(rriskModel@settings@N,min=", item@plausimin, 
                     ",max=", item@plausimax, "),byrow=TRUE)")
    relaxc.test <- paste0("matrix(ncol=", batch, 
                          ",data=runif(rriskModel@settings@Ntest,min=", 
                          item@plausimin, ",max=", item@plausimax, "),byrow=TRUE)")
    relaxcValues <- try(eval(parse(text = relaxc.test)), silent = TRUE)
    
    if (inherits(relaxcValues, "try-error")) {
      #cat("Relax command expression", relaxc, "could not be evaluated !\n")
    } else {
      #cat("\n***************************************\n") 
      #cat("Relax command for", item@name) 
      #cat("\n***************************************\n")
      #cat("\nDimension of test evaluation: ", dim(relaxcValues), "\n")
      #cat("\nitem@relaxc=", relaxc, "\n")
      #relaxcValues <- data.frame(relaxcValues)
      #names(relaxcValues) <- item@name
      #cat("\nGenerated item values (relax command):\n")
      #print(head(relaxcValues))
      #cat("...\n")
      #cat("\nSummary statistics of item values (relax command):\n")
      #print(summary(relaxcValues), digits = 4)
      #cat("\n")
      item@relaxc <- relaxc
    }
    
    return(item) # ?
  } # end if
  
  item
} 