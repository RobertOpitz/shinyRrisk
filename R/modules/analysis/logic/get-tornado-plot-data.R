
#' @name getTornadoPlotData
#' @title getTornadoPlotData
#' @description Creates data for tornado plot.
#' @param rriskModel Model object.
#' @return List of plot data.
#' @export
getTornadoPlotData <- function(rriskModel) {
  
  cat("getTornadoPlotData\n")
   
  # if there is nothing, then there is also nothing to do
  if(length(rriskModel@output@fullout.1d)  == 0 & 
     length(rriskModel@output@relaxout.1d) == 0)
    return(list())
  
  #-----------------------------------------------------------------------------
  # identify available mcrv items, available OF items and available uncert 
  # (mcrv and 'u', 'uv') items
  #-----------------------------------------------------------------------------
  OF.items    <- c()
  mcrv.items  <- c()
  mcrv.col    <- c()
  mcrv.border <- c()
  #mcrv <- list(items = NULL, col = NULL, border = NULL) #? 
  #unc.items <- c() # dead code
  mycol       <- rriskModel@settings@mycol
  
  #---Begin: loop items---------------------------------------------------------
  for (item in rriskModel@items@items) {
    # get OF items
    if(item@rolecode == "OF")
      OF.items <- c(OF.items, item@name) 
    # get mcrv items of type "u" oder "uv"
    if (item@typecode == "mcrv") {
      if (item@stratum != "")
        cat("Statified mcrv item'", item@name, "'will be ignored.\n")
      else {
        if (is.null(item@data) & item@fullc == "")
          cat("Not evaluated mcrv item'", item@name, "'will be ignored.\n")
        else {
          # now do something usefull
          mcrv.items  <- c(mcrv.items, item@name)
          mcrv.col    <- c(mcrv.col, "white")
          mcrv.border <- c(mcrv.border, mycol)
        
          if (item@rolecode == "u" | item@rolecode == "uv") {
            #unc.items                        <- c(unc.items, item@name) #dead code
            mcrv.col[length(mcrv.col)]       <- mycol
            mcrv.border[length(mcrv.border)] <- "white"
          }
        }
      }
    }
  }
  #---End: loop over items------------------------------------------------------
    
  # check if we catched something for OF.items
  if(length(OF.items) == 0) {
    cat("Tornado plot cannot be created,", 
        "there is no 'OF' item defined in the model!")
    return(list())
  }
    
  # check if we catched something for mcrv.items
  # (this applies then for mcrv.col & mcrv.border)
  if(length(mcrv.items) == 0) {
    cat("Regression tree plot cannot be created,",
        "there is no uncertainty mcrv item(s) defined in the model!")
    return(list())
  } else{
    mcrv.col    <- rev(mcrv.col)
    mcrv.border <- rev(mcrv.border)
  }
    
  #-----------------------------------------------------------------------------
  # evaluate data for tornado charts
  #-----------------------------------------------------------------------------
  # get data of 1d simulation for full model
  fullout.1d <- rriskModel@output@fullout.1d
  fullout.1d <- as.data.frame(fullout.1d[c(mcrv.items, OF.items)])
  
  # get data of 1d simulation for relaxed model
  relaxout.1d <- rriskModel@output@relaxout.1d
  relaxout.1d <- as.data.frame(relaxout.1d[c(mcrv.items, OF.items)])

  # transform 1d simulation data accoring to model settings
  if(rriskModel@settings@trans == "rank") {
    
    fullout.1d    <- apply(fullout.1d,  2, rank)
    relaxout.1d   <- apply(relaxout.1d, 2, rank)
    tornado.xlab0 <- "using rank transformated data"
    
  } else if(rriskModel@settings@trans == "z-score") {
    
    fullout.1d    <- apply(fullout.1d,  2, scale)
    relaxout.1d   <- apply(relaxout.1d, 2, scale)
    tornado.xlab0 <- "using z-transformed data"
    
  } else if(rriskModel@settings@trans == "identity") {
    tornado.xlab0 <- "using original data"
  }
    
  #---BEGIN: loop Create tornado plot for each 'OF' item------------------------
  for (OF in OF.items) { # this loop runs only on time, 
                         # because of the return at the end of the loop body
                         # WHY?
     
    OF.index  <- which(colnames(fullout.1d) == OF) # does colnames change during looping?
    
    OF.full   <- fullout.1d[, OF.index]
    data.full <- as.matrix(subset(fullout.1d, select = -c(OF.index)))
     
    OF.relax   <- relaxout.1d[, OF.index]
    data.relax <- as.matrix(subset(relaxout.1d, select = -c(OF.index)))

    # model for sensitivity analysis, choices by settings: 
    # "correlation" (default), "regression"
    if(rriskModel@settings@sens == "correlation") { # does that change during loop?
      
      effect.full  <- rep(NA, length(mcrv.items))
      effect.relax <- rep(NA, length(mcrv.items))
      for (i in seq_along(mcrv.items)) {
        effect.full[i]  <- cor(data.full[, i], OF.full)
        effect.relax[i] <- cor(data.relax[,i], OF.relax)
      }
      tornado.xlab <- paste("Correlation coefficients", tornado.xlab0)
      
    } else if(rriskModel@settings@sens == "regression") {

      effect.full  <- lm.fit(x = data.full,  y = OF.full)$coeff
      effect.relax <- lm.fit(x = data.relax, y = OF.relax)$coeff
      tornado.xlab <- paste("Regression coefficients", tornado.xlab0)
      
    } else {
      effect.full  <- rep(NA, length(mcrv.items))
      effect.relax <- rep(NA, length(mcrv.items))
    }
    
    names(effect.full)  <- mcrv.items
    names(effect.relax) <- mcrv.items
     
    xlim <- c(min(as.logical(effect.full), 
                  as.logical(effect.relax), na.rm = TRUE),
              max(as.logical(effect.full), 
                  as.logical(effect.relax), na.rm = TRUE))

    if (xlim[1] == xlim[2]) {
      if (xlim[1] > 0)
        xlim <- c(0, xlim[1])
      else if (xlim[1] < 0)
        xlim <- c(xlim[1], 0)
    }
    
    # create tornado plots
    par(mfrow = c(1,2), oma = c(2, 1, 5, 1))
      
    full <- list(data   = as.data.frame(rev(effect.full)), 
                 horiz  = TRUE,
                 main   = "Full model",
                 col    = mcrv.col,
                 xlim   = xlim,
                 border = mcrv.border,
                 xlab   = "",
                 xaxt   = "s",
                 las    = 1,
                 lwd    = 2)

    relax <- list(data   = as.data.frame(rev(effect.relax)),
                  horiz  = TRUE,
                  main   = "Relaxed model",
                  col    = mcrv.col,
                  xlim   = xlim,
                  border = mcrv.border,
                  xlab   = "",
                  xaxt   = "s",
                  las    = 1,
                  lwd    = 2)
          
    # the loop is prematurely left for some reason,
    # maybe because only one OF-item can be dealt with at the moment
    return(list("full" = full, "relax" = relax))
  }
  #---END: loop Create tornado plot for each 'OF' item--------------------------
}
