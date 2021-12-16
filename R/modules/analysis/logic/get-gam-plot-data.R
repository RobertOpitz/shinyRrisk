#' @name getGAMPlotData
#' @title getGAMPlotData
#' @description Creates data for GAM plot.
#' @param rriskModel Model object.
#' @return List of plot data.
#' @export
getGAMPlotData <- function(rriskModel) {
  cat("getGAMPlotData\n")
  
  # nothing to do, return empty plotting list
  if(rriskModel@output@OFname.2d == "")
    return(list())
  
  if (length(rriskModel@items@items) == 0)
    stop(paste("Regression tree plot cannot be created,",
               "the list of model items is empty!"), call. = FALSE)
  
  #-----------------------------------------------------------------------------
  # identify available OF items and available uncert 
  # (mcrv and 'u', 'uv') mcrv items
  #-----------------------------------------------------------------------------
  mcrv.unc.items <- c()
  for(item in rriskModel@items@items) {
    if (item@typecode == "mcrv" & 
        (item@rolecode == "u" | item@rolecode == "uv")) {
      mcrv.unc.items <- c(mcrv.unc.items, item@name)
    }
  }
  
  # internal function
  getPlotData <- function(y, x, xlab, ylab, mainText, col) {
    #cat("length(y) = ", length(y), "\n")
    #print(y)
    #selec_vals <- seq(1, length(y), length.out = length(y)/2)
    ylab <- paste0("Smoothed effect on the outcome function ", ylab)
    plotData <- list("gam"       = gam(y ~ s(x)), #gam(y[selec_vals] ~ s(x[selec_vals])), # takes very long; downsampling x and y?
                     "main"      = mainText, 
                     "shade"     = TRUE, 
                     "xlab"      = xlab, 
                     "ylab"      = ylab, 
                     "residuals" = FALSE, 
                     "rug"       = FALSE, 
                     "shade.col" = col)
    plotData
  }

  # create list with plotting data, and return this list
  print("gamplot apply")
  cat("length(mcrv.unc.itmes) = ", length(mcrv.unc.items), "\n")
  this_time <- Sys.time()
  result <- parallel::parLapply(cl = cl,
    mcrv.unc.items,
    function(item, rmo, rms, y.1d, y.2d){
      #-------------------------------------------------------------------------
      # non-parametric regression for 1d simulation
      #-------------------------------------------------------------------------
      x.1d <- rmo@fullout.1d[[item]]
      plotData1d <- getPlotData(y.1d, x.1d, 
                                xlab = item, ylab = "P", 
                                mainText = "1st order simulation", 
                                col = rms@mycol)
      #-------------------------------------------------------------------------
      # non-parametric regression for 2d simulation
      #-------------------------------------------------------------------------
      x.2d <- rmo@uncitems.2d[,item]
      x.2d <- as.numeric(rep(x.2d, rms@N))
      plotData2d <- getPlotData(y.2d, x.2d, 
                                xlab = item, ylab = "P", 
                                mainText = "2nd order simulation", 
                                col = rms@mycol)
      list("1d" = plotData1d, "2d" = plotData2d)
    }, 
    rriskModel@output, rriskModel@settings, # rmo & rms
    rriskModel@output@fullout.1d[[rriskModel@output@OFname.2d]], # y.1d
    as.numeric(t(rriskModel@output@fullout.2d)) # y.2d
  )
  print(Sys.time() - this_time)
  result
}
