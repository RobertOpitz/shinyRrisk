#' @name menu.define.rsrv
#' @aliases menu.define.rsrv
#' @title Non-executable auxiliary function
#' @usage menu.define.rsrv(item,rriskModel,menuLevel=1)
#' @param item ...
#' @param rriskModel ...
#' @param menuLevel ...
#' @keywords items
#' @export

# EDIT(1)
menu.define.rsrv <- function(item, rriskModel, menuLevel = 1, itemId) {
  cat("menu.define.rsrv\n")
  
  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(invisible(item)))
  
  #-----------------------------------------------------------------------------
  # define some help variables
  #-----------------------------------------------------------------------------
  levelTabulator <- paste(rep("\t", menuLevel), sep = "", collapse = "")
  levelTabulator <- paste("\n", levelTabulator, sep = "")
  
  # get all data sets assosiated with the current model
  availableData <- c()
  availableDataRCODE <- c()
  availableMcrvFnrv.names <- c()
  availableMcrvFnrv.data <- c()
  items <- rriskModel@items@items
  
  if (length(items) > 0) {
    for (i in 1:length(items)) {
      if (items[[i]]@typecode == "data") {
        
        #----------------------------------------------------------------------
        # get all data items
        #----------------------------------------------------------------------
        item.name <- items[[i]]@name
        var.names <- names(items[[i]]@data)
        availableData.temp <- paste(item.name, var.names, sep = "$")
        availableDataRCODE.temp <- paste("rriskModel@items@items[[", i, "]]@data$", var.names, sep = "")
        availableData<-c(availableData,availableData.temp)
        availableDataRCODE<-c(availableDataRCODE,availableDataRCODE.temp)
        names(availableDataRCODE)<-availableData.temp
        
      } else if (is.element(items[[i]]@typecode,c("mcrv", "fnrv"))) {
        
        #----------------------------------------------------------------------
        # evaluate/get all mcrv and fnrv items
        #----------------------------------------------------------------------
        if(!is.null(items[[i]]@data) | items[[i]]@fullc!=""){
          availableMcrvFnrv.names <- c(availableMcrvFnrv.names,items[[i]]@name)
        }
      }
    } # end for
    
    #---------------------------------------------------------------------------
    # get evaluated values of all mcrv and fnrv items
    #---------------------------------------------------------------------------
    # item<-new(itemClass)
    dataForWith <- get.dataForWith(item,
                                   rriskModel,
                                   run = FALSE,
                                   catEval = FALSE)
    
    if (inherits(dataForWith, "try-error")) {
      cat("Full command expression", item@fullc, "could not be evaluated !\n")
    } else { # Weise Item-Slots 'definition' und 'fullc' Inhalte zu
      availableMcrvFnrv.data <- dataForWith[availableMcrvFnrv.names]
    } # end if (inherits(dataForWith, "try-error")){
  } # end  if(length(items)>0){
  
  if (length(c(availableData, names(availableMcrvFnrv.data))) == 0) {
    stop("The model does not contain any data, mcrv or fnrv item!", call. = FALSE)
  } # end if(length(availableData, names(availableMcrvFnrv.data))==0){
  
  choices <- c(availableData, names(availableMcrvFnrv.data), "Exit dialog")
  input <- 99
  
  # EDIT(1)
  # while (!is.element(input, seq(1:length(choices)))) {
  
  # EDIT(2)
  # input <- mymenu(title = "Please choose data set for resampling", choices = choices, help = "No further help available", levelTabulator = levelTabulator)
  input <- itemId
  
  input.old <- as.numeric(input)
  input <- choices[as.numeric(input)]
  
  if (input %in% availableData) {
    
    data2fitname<-input
    data2fit<-eval(parse(text=availableDataRCODE[input.old]))
    #View(data2fit)
    #print(head(data2fit))
    cat(data2fitname,": ",head(data2fit),"...\n")
    #item<-rsrv.resample(data2fit,data2fitname,item,rriskModel)
    item <- rsrv.resample(data2fitname, item, rriskModel)
    break()
    
  } else if( input %in% availableMcrvFnrv.names) {
    data2fit<-availableMcrvFnrv.data[[input]]
    data2fitname<-input
    cat(data2fitname,": ",head(data2fit),"...\n")
    item <- rsrv.resample(data2fitname, item, rriskModel)
    break()
  } else if (input.old == length(choices)) {
    break()
  }
  # input<-99
  
  # EDIT(1)
  # } # end while
  
} # end of function menu.define.rsrv()