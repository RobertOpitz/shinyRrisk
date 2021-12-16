#' @name run2dsimulation
#' @aliases run2dsimulation
#' @title Non-executable auxiliary function
#' @usage run2dsimulation(rriskModel)
#' @param rriskModel ...
#' @param menuLevel ...
#' @keywords run
#' @export

run2dsimulation <- function(rriskModel, ofId) {
  cat("run2dsimulation\n")
  
  # run 1d simulation
  rriskModel <- run1dsimulation(rriskModel)
  
  # set some helping variables
  N2d   <- rriskModel@settings@N2d
  items <- rriskModel@items@items
  if (length(items) == 0)
    stop("Current model cannot be run, the list of model items is empty!")
  
  quantile_vec <- c(0.0001, 0.001, 0.01, 0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 
                    0.975, 0.99, 0.999, 0.9999)
  quantile_names <- paste0("Qt", format(100 * quantile_vec, 
                                        scientific = FALSE, 
                                        trim = TRUE))
  perc_vec <- c(seq(from = 0, to = 1, by = 0.01), quantile_vec)
  
  #---BEGIN: internal helper functions------------------------------------------
  get_quantiles <- function(data, fun) {
    if (missing(fun))
      signif(quantile(x = data,
                      probs = perc_vec,
                      na.rm = TRUE,
                      type = 7),
             digits = 4)
    else
      signif(quantile(x = apply(data, 2, fun),
                      probs = quantile_vec,
                      na.rm = TRUE,
                      type = 7),
             digits = 4)
  }
  
  insert_columns <- function(x, columns, after_column) {
    # column bind new columsn to old columns
    new_x  <- cbind(x, columns)
    # get column names of old and new columns
    x_cols <- colnames(x)
    c_cols <- colnames(columns)
    # insert new columns to old columns at position needed
    new_x_cols <- append(x_cols, c_cols, after_column)
    # re-order columns of the new columns
    new_x <- new_x[, new_x_cols]
    new_x
  }
  #---END: internal helper functions--------------------------------------------
  
  # get OF, v and uv items
  OF.items <- c()
  v.items  <- c()
  uv.items <- c()
  
  for (i in seq_along(items)) {
    # collect outcome- and remain items
    if (items[[i]]@rolecode == "OF")
      OF.items <- c(OF.items, items[[i]]@name)
    
    if (items[[i]]@typecode == "mcrv") {
      # collect variability and uncertainty items
      if (items[[i]]@rolecode == "v")
        v.items  <- c(v.items,  items[[i]]@name)
      else if(items[[i]]@rolecode=="u" | items[[i]]@rolecode=="uv") {
        uv.items <- c(uv.items, items[[i]]@name)
        items[[i]]@fullc <- gsub(x           = items[[i]]@fullc, 
                                 pattern     = "rriskModel@settings@N", 
                                 replacement = "1")
      } 
    }
  }
  
  # if nothing was collected, than nothing needs to be done
  # return the input model
  if (length(v.items)  == 0 & 
      length(uv.items) == 0 & 
      N2d == 0) {
    warning(paste("Model does not contain any 'uv', 'u' or any 'v' and the", 
                  "length of the 2d simulation is probably equal to zero!\n"),
            immediate. = TRUE)
    return(rriskModel)
  }
  
  # create a temp model
  rriskModel.temp <- rriskModel
  # set items of temp model
  rriskModel.temp@items@items <- items
  
  # get OF items für 2d simulation
  if (length(OF.items) > 1)
    OF <- OF.items[ofId]
  else
    OF <- OF.items[1]
  
  # OF.q collects statistics of all outcome function items for display
  # it has 13 rows for quantiles and cols equal to number of OF items
  OF2d.q <- as.data.frame(matrix(data = NA,
                                 nrow = 13,
                                 ncol = 3))
  rownames(OF2d.q) <- quantile_names
  colnames(OF2d.q) <- c(paste0(OF, "(50)"),
                        paste0(OF, "(2.5)"),
                        paste0(OF, "(97.5)"))
  
  OF.cdf       <- matrix(nrow = N2d, ncol = length(perc_vec))
  OF.cdf.CI    <- matrix(nrow = 2,   ncol = length(perc_vec))
  colnames(OF.cdf.CI) <- paste0("Q", 100 * perc_vec)
  
  # other helping objects
  fullout.2d     <- vector(mode = "list", length = N2d)
  uncertainty.2d <- matrix(nrow = N2d, ncol = length(uv.items))
  colnames(uncertainty.2d) <- uv.items
  
  withProgress(min     = 0,
               max     = N2d,
               value   = 0,
               message = "Running",
               detail  = "1 %",
               style   = shiny::getShinyOption("progress.style", 
                                               default = "notification"),
               session = shiny::getDefaultReactiveDomain(), {
                 for (i in seq_len(N2d)) {
                   results.2d <- get.dataForWith(rriskModel = rriskModel.temp,
                                                 fullc      = TRUE, 
                                                 run        = TRUE, 
                                                 run2d      = TRUE)
                 
                   OF.cdf[i,]         <- get_quantiles(results.2d[[OF]]) 
                   fullout.2d[[i]]    <- results.2d[[OF]]
                   uncertainty.2d[i,] <- unlist(results.2d[uv.items])
                 
                   # set progress bar
                   incProgress(amount  = 1,
                               detail  = paste(round(i / N2d * 100), "%"),
                               session = shiny::getDefaultReactiveDomain())
                 }
               }) # end withProgress
  
  # change fullout.2d from a list to a matrix  
  fullout.2d <- matrix(unlist(fullout.2d), ncol = N2d)
  
  # calculate quantile of the outcome function
  OF2d.q[,1] <- get_quantiles(data = fullout.2d, 
                              fun  = median)
  OF2d.q[,2] <- get_quantiles(data = fullout.2d, 
                              fun  = function(x) quantile(x, probs = 0.025))
  OF2d.q[,3] <- get_quantiles(data = fullout.2d, 
                              fun  = function(x) quantile(x, probs = 0.95))
  # test
  for (perc in seq_along(perc_vec))
    OF.cdf.CI[,perc] <- quantile(OF.cdf[,perc], probs = c(.025, .975))
  
  # insert columns from OF2d.q to OF1d.q at position which(names(OF1d.q) == OF)
  OF1d.q <- rriskModel@output@summaries
  OF.q   <- insert_columns(x            = OF1d.q, 
                           columns      = OF2d.q, 
                           after_column = which(names(OF1d.q) == OF))
  
  # save simulation results to the rrisk model
  rriskModel@output@OFname.2d   <- OF
  rriskModel@output@fullout.2d  <- fullout.2d
  rriskModel@output@uncitems.2d <- uncertainty.2d
  rriskModel@output@OFcdfCI     <- OF.cdf.CI
  rriskModel@output@summaries   <- OF.q

  rriskModel
}