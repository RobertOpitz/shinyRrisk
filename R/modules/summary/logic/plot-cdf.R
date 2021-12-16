#' @description A function that creates a cumulative distribution of the main outcome function based on 2d-simulation.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name plotCDF
#' @aliases plotCDF
#' @title Function that draws a cumulative distribution of the main outcome function based on 2d-simulation
#' @usage plotCDF(rriskModel)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param pdfGraph decides whether the plot is as PDF data to be saved
#' @keywords graphs
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' rriskModel<-run(rriskModel)
#' plotCDF(rriskModel) }

plotCDF <- function(rriskModel) {
  
  cat("plotCDF\n")
  
  # if there is nothing, ther is also nothing to plot
  if (length(rriskModel@items@items) == 0)
    return(NULL)
  
  if (is.null(rriskModel@output@fullout.2d) &
      length(rriskModel@output@fullout.1d) == 0)
    return(NULL)
  
  # get OF items
  OFitems <- c()
  OFunits <- c()
  for (item in rriskModel@items@items) {
    if (item@rolecode == "OF"){
      OFitems <- c(OFitems, item@name)
      OFunits <- c(OFunits, item@unit)
    }
  }
  
  if (length(OFitems) == 0)
    return(NULL)
  
  #---BEGIN: create cdf plot of each OF item------------------------------------
  # if there is a 2d fullout model, transpose it for 2d cdf plotting
  if (!is.null(rriskModel@output@fullout.2d))
    transposed_fullout.2d <- t(rriskModel@output@fullout.2d)
  
  for (i in seq_along(OFitems)) {
    if (rriskModel@output@OFname.2d == OFitems[i]) # cdf plot for 2d simulations
      OF.resampled <- base::sample(x       = transposed_fullout.2d,
                                   size    = 1000,
                                   replace = TRUE)
    else # cdf plot for 1d simulations
      OF.resampled <- base::sample(x  = rriskModel@output@fullout.1d[OFitems[i]][[1]],
                                   size    = 1000,
                                   replace = TRUE)
    
    xlab <- OFitems[i]
    OFunit <- OFunits[which(OFitems == xlab)]
    xlab <- paste0(xlab, " [", OFunit, "]")
    
    #---BEGIN: create plot------------------------------------------------------
    plot(x         = ecdf(OF.resampled),
         main      = "",
         ylab      = paste0("cdf(", OFitems[i], ")"),
         xlab      = xlab,
         do.points = FALSE,
         verticals = TRUE,
         lty       = 2)
    
    # cdf plot for 2d simulations
    if (rriskModel@output@OFname.2d == OFitems[i]) {
      for (j in 1:101) #see also 1d & 2d simulation: seq(0, 1, 0.01)
        lines(x    = rriskModel@output@OFcdfCI[,j],
              y    = rep((j-1)/100, 2),
              col  = rriskModel@settings@mycol,
              lend = 1,
              lwd  = 5)
      lines(ecdf(OF.resampled), pch = ".")
    }
    #---END: create plot--------------------------------------------------------
  }
  #---END: create cdf plot of each OF item--------------------------------------
  
  NULL
}