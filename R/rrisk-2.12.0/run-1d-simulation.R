#' @name run1dsimulation
#' @aliases run1dsimulation
#' @title Non-executable auxiliary function
#' @usage run1dsimulation(rriskModel)
#' @param rriskModel ...
#' @keywords run
#' @export

run1dsimulation <- function(rriskModel) {
  cat("run1dsimulation\n")
  
  # reset simulation results
  rriskModel <- resetSimulationResults(rriskModel)
  
  # resample strv of stra-Items
  rriskModel <- resampleSTRATA(rriskModel)
  
  # some helping variables
  items <- rriskModel@items@items
  if (length(items) == 0)
    stop("Current model cannot be run, the list of model items is empty!")
  
  quantile_vec <- c(0.0001, 0.001, 0.01, 0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 
                    0.975, 0.99, 0.999, 0.9999)
  quantile_names <- paste0("Qt", format(100 * quantile_vec, 
                                        scientific = FALSE, 
                                        trim = TRUE))
  
  # get OF items
  availableOFs <- c()
  for (item in items)
    if (item@rolecode == "OF")
      availableOFs <- c(availableOFs, item@name)
  
  # breche ab falls die Anzahl von Outcome-Items null ist
  if (length(availableOFs) == 0)
    stop(paste("At least one outcome function (OF) needs to be defined", 
               "before the model can be run!\n",
               "The current model does not contain any 'OF' item."))
  
  # run 1d simulation
  fullout.1d  <- list()
  relaxout.1d <- list()
  iterat      <- rriskModel@settings@N
  
  fullout.1d <- get.dataForWith(rriskModel = rriskModel, 
                                fullc      = TRUE, 
                                run        = TRUE)
  
  relaxout.1d <- get.dataForWith(rriskModel = rriskModel, 
                                 fullc      = FALSE, 
                                 run        = TRUE)
  
  # OF.q collects statistics of all outcome function items for display
  # it has 13 rows for quantiles and cols equal to number of OF items
  OF1d.q <- as.data.frame(matrix(data = NA,
                                 nrow = 13,
                                 ncol = length(availableOFs)))
  rownames(OF1d.q) <- quantile_names
  colnames(OF1d.q) <- availableOFs
  
  for (i in seq_along(availableOFs)) {
    availableOFs.temp <- availableOFs[[i]]
    fullout.1d.OFtemp <- fullout.1d[[availableOFs.temp]]
    
    OF1d.q[, i] <- signif(quantile(x     = fullout.1d.OFtemp,
                                   probs = quantile_vec,
                                   na.rm = TRUE, 
                                   type  = 7), 
                          digits = 4)    
    
    # was ist das denn?
    if(iterat < 50000) {
      OF1d.q[13,i] <- NA
      if (iterat < 40001)    OF1d.q[1, i]  <- NA
      if (iterat < 5000)     OF1d.q[12,i]  <- NA
      if (iterat < 4001)     OF1d.q[2, i]  <- NA
      if (iterat < 500)      OF1d.q[11,i]  <- NA
      if (iterat < 401)      OF1d.q[3, i]  <- NA
      if (iterat < 200)      OF1d.q[10,i]  <- NA
      if (iterat < 161)      OF1d.q[4, i]  <- NA
      if (iterat < 100)      OF1d.q[9, i]  <- NA
      if (iterat < 81)       OF1d.q[5, i]  <- NA
      if (iterat < 50)       OF1d.q[8, i]  <- NA
      if (iterat < 41)       OF1d.q[6, i]  <- NA
      if (iterat < 10)       OF1d.q[5, i]  <- NA
    }
  } # end for
  
  # save 1d simualtion results into the model object
  rriskModel@output@fullout.1d  <- fullout.1d
  rriskModel@output@relaxout.1d <- relaxout.1d
  rriskModel@output@summaries   <- OF1d.q
  
  # if we have no full out 1d simulation, there is nothing else to do
  # return with the current model
  if (length(fullout.1d) == 0)
    return(rriskModel)
  
  # save evaluated item results in data-Slot (if possible)
  for (i in seq_along(rriskModel@items@items)) {
    
    itemname <- rriskModel@items@items[[i]]@name
    dataslot <- rriskModel@items@items[[i]]@data
    typecode <- rriskModel@items@items[[i]]@typecode
    
    if (itemname %in% names(fullout.1d)) {
      
      fullout.1d.item <- fullout.1d[[itemname]]
      
      if (typecode == "numv")
        dataslot <- fullout.1d.item
      else if (typecode %in% c("fnrv","rsrv")) {
        fullout.1d.item           <- data.frame(fullout.1d.item)
        colnames(fullout.1d.item) <- itemname
        dataslot <- summary(fullout.1d.item)
      } else if (typecode == "mcrv"){
        fullout.1d.item <- data.frame(fullout.1d.item)
        if (length(fullout.1d.item) == 1)
          colnames(fullout.1d.item) <- itemname
        dataslot <- summary(fullout.1d.item)
      } else if (typecode == "bsrv")
        dataslot <- summary(fullout.1d.item)
      else
        dataslot <- NULL
      
      # update data-Slot
      rriskModel@items@items[[i]]@data <- dataslot  
    }
  }
  
  rriskModel
}