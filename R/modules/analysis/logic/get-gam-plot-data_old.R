#' @name getGAMPlotData
#' @title getGAMPlotData
#' @description Creates data for GAM plot.
#' @param rriskModel Model object.
#' @return List of plot data.
#' @export
getGAMPlotData <- function(rriskModel) {
  cat("getGAMPlotData\n")
  
  mcrv.unc.items <- c()
  plotsData <- list()

  if(rriskModel@output@OFname.2d != "") {

    #-----------------------------------------------------------------------------
    # identify available OF items and available uncert (mcrv and 'u', 'uv') mcrv items
    #-----------------------------------------------------------------------------
    if(length(rriskModel@items@items) > 0) {
      for(i in 1:length(rriskModel@items@items)) {
        if(rriskModel@items@items[[i]]@typecode == "mcrv") { 
          if(rriskModel@items@items[[i]]@rolecode == "u" | rriskModel@items@items[[i]]@rolecode == "uv") { 
            mcrv.unc.items <- c(mcrv.unc.items, rriskModel@items@items[[i]]@name)
          }
        }                                                                       
      }
    } else stop("Regression tree plot cannot be created, the list of model items is empty!", call. = FALSE)

    getPlotData <- function(y, x, xlab, ylab, mainText, col) {
      ylab <- paste("Smoothed effect on the outcome function ", ylab, sep = "")
      plotData <- list(
        "gam" = gam(y~s(x)), 
        "main" = mainText, 
        "shade" = TRUE, 
        "xlab" = xlab, 
        "ylab" = ylab, 
        "residuals" = FALSE, 
        "rug" = FALSE, 
        "shade.col" = col
      )

      return(plotData)
    }

    for(i in 1:length(mcrv.unc.items)) {

      item.temp <- mcrv.unc.items[i]

      #-----------------------------------------------------------------------------
      # non-parametric regression for 1d simulation
      #-----------------------------------------------------------------------------
      x.1d <- rriskModel@output@fullout.1d[[item.temp]]
      y.1d <- rriskModel@output@fullout.1d[[rriskModel@output@OFname.2d]]

      plotData1d <- getPlotData(y.1d, x.1d, xlab = item.temp, ylab = "P", mainText = "1st order simulation", col = rriskModel@settings@mycol)

      #-----------------------------------------------------------------------------
      # non-parametric regression for 2d simulation
      #-----------------------------------------------------------------------------
      y.2d <- as.numeric(t(rriskModel@output@fullout.2d))
      x.2d <- rriskModel@output@uncitems.2d[,item.temp]
      x.2d <- as.numeric(rep(x.2d,rriskModel@settings@N))

      plotData2d <- getPlotData(y.2d, x.2d, xlab = item.temp, ylab = "P", mainText = "2nd order simulation", col = rriskModel@settings@mycol)
      plotsData[[i]] <- list("1d" = plotData1d, "2d" = plotData2d)
    }

  } # else: Non-parametric regression plots are omitted because 2d simulation was not conducted.

  return(plotsData)
}
