#' @description A function that creates a convergence plot.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name plotOFConvergence
#' @aliases plotOFConvergence
#' @title Function that draws a convergence plot
#' @usage plotOFConvergence(rriskModel)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param pdfGraph decides whether the plot is as PDF data to be saved
#' @keywords graphs
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' rriskModel<-run(rriskModel)
#' plotOFConvergence(rriskModel)  }

plotOFConvergence <- function(rriskModel) {

  cat("plotOFConvergence\n")
  
  if (length(rriskModel@items@items) == 0) {
    warning("No convergence plot can be created, the list of model items is empty!",
            immediate. = TRUE)
    return(NULL)
  }
  
  if (length(rriskModel@output@fullout.1d) == 0) {
    warning("1d simulation of the full command has not be run!",
            immediate. = TRUE)
    return(NULL)
  }
  
  # set some helping variables
  settings  <- rriskModel@settings
  N         <- settings@N
  k         <- round(N/100)
  Iteration <- c(seq(from = k, to = N, by = k), N)
  max_iter  <- length(Iteration)
  
  # get OF items # there is so far only ONE output-function item considered by rrisk!
  OFitems <- c()
  for (item in rriskModel@items@items)
    if (item@rolecode == "OF")
      OFitems <- c(OFitems, item@name)
  
  #---BEGIN: create convergence output------------------------------------------
  OFnumber <- 0
  this_names <- names(rriskModel@output@fullout.1d)
  for (j in seq_along(OFitems)) {
    if (!(OFitems[j] %in% this_names))
      next
    
    OFnumber  <- OFnumber + 1
    OF.values <- rriskModel@output@fullout.1d[[OFitems[j]]]
    
    mean_curve     <- sapply(Iteration,
                             function(iter) mean(OF.values[1:iter]))
    mean_ybar      <- mean_curve[max_iter]
    mean_tolerance <- mean_ybar + c(-1, 1) * settings@abserror
    
    median_curve     <- sapply(Iteration,
                               function(iter) median(OF.values[1:iter]))
    median_ybar      <- median_curve[max_iter]
    median_tolerance <- median_ybar + c(-1, 1) * settings@abserror

    # create plot
    ylabText <- paste0("median and mean of OF item =", OFitems[j])

    # plot results for median
    plot(x    = Iteration,
         y    = median_curve,
         type = "l",
         ylim = range(median_curve, mean_curve, 
                      median_tolerance, mean_tolerance),
         main = "",
         xlab = "Iteration",
         ylab = ylabText,
         col  = "blue")#settings@mycol)
    abline(h = median_ybar, lty = 3, col = settings@mycol)
    abline(h = median_tolerance, lty = 2)
    
    # add results for mean
    lines(x   = Iteration,
          y   = mean_curve,
          col = "blue",
          lty = 5)
    abline(h = mean_ybar, lty = 3, col = settings@mycol)
    abline(h = mean_tolerance, lty = 2)
    
    # add legend
    legend("topright", 
           legend = c("median","mean"), 
           lty    = c(1,5),
           col    = "blue",
           inset  = c(0, -0.025),
           horiz  = TRUE,
           bty    = "n")
  }
  #---END: create convergence output--------------------------------------------
  
  if (OFnumber == 0)
    stop("There is no outcome items in the 1d simulation results!")
  
  NULL
}
