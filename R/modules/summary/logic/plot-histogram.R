#' @description A function that creates a histogram.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name plotOFHistogram
#' @aliases plotOFHistogram
#' @title Function that draws a histogram
#' @usage plotOFHistogram(rriskModel)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param pdfGraph decides whether the histogram is as PDF data to be saved
#' @keywords graphs
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' rriskModel<-run(rriskModel)
#' plotOFHistogram(rriskModel)  }

plotOFHistogram <- function(rriskModel) {
  
  cat("plotOfHistogram\n")
  
  # nothing in the model to plot
  if (length(rriskModel@items@items) == 0) {
    warning("No convergence plot can be created, the list of model items is empty!",
            immediate. = TRUE)
    return(NULL)
  }
  
  # no model, no plot
  if (length(rriskModel@output@fullout.1d) == 0) {
    warning("1d simulation of the full command has not be run!",
            immediate. = TRUE)
    return(NULL)
  }
  
  # get OF items and OF units # so far only ONE output function item is considered by rrisk!
  OFitems <- c()
  OFunits <- c()
  for (item in rriskModel@items@items) {
    if (item@rolecode == "OF"){
      OFitems <- c(OFitems, item@name)
      OFunits <- c(OFunits, item@unit)
    }
  }
  
  # create histogram of each OF item
  OFnumber <- 0
  this_names <- names(rriskModel@output@fullout.1d)
  #---BEGIN: plot loop----------------------------------------------------------
  for (i in seq_along(OFitems)) {
    if (!(OFitems[i] %in% this_names))
      next # nothing to do, try the next item
    
    OF.values <- rriskModel@output@fullout.1d[[OFitems[i]]]
    OFnumber  <- OFnumber + 1
    
    xlabText <- paste0(OFitems[i], " [",OFunits[i],"]")
    
    hist_plot <- hist(x      = OF.values, 
                      freq   = FALSE, 
                      main   = "", 
                      xlab   = xlabText, 
                      col    = rriskModel@settings@mycol, 
                      border = rriskModel@settings@mycol,
                      breaks = 200)

    # get smoothed line for histogram
    smoothing_line <- density(x      = OF.values,
                              kernel = "epanechnikov", 
                              adjust = 0.25)
    # exclude values of smoothed line, that are outside of the min and 
    # max values of the histogramm
    n <- which(smoothing_line$x >= hist_plot$mids[1] &
               smoothing_line$x <= hist_plot$mids[length(hist_plot$mids)])
    # plot smoothed line on top of histogram
    lines(x   = smoothing_line$x[n],
          y   = smoothing_line$y[n],
          col = "blue")
  }
  #---END: plot loop------------------------------------------------------------
  
  if (OFnumber == 0)
    stop("There is no outcome items in the 1d simulation results!")
  
  NULL
}
