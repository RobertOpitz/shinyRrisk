################################################################################
################################################################################
# R-Funktionen zum rriskDistributions Paket (rrisk Projekt)
# Auftraggeber: Bundesinstitut für Risikobewertung, Berlin
# Auftragnehmer: Stat-Up, München
# ------------------------------------------------------------------------------
# Autor: Natalia Belgorodski, Stat-Up
# Anpassungen von Lutz Göhring, Lutz Göhring Consulting
#   Update: 05.05.2015 (Anpassungen an CRAN-Forderungen)
# Weitere Anpassungen von Matthias Flor, BfR
#   Update: 16.03.2016 (erneute Anpassungen an CRAN-Forderungen)
################################################################################
################################################################################

is.error <- function(x) inherits(x, "try-error")

assert_fit.perc_input <- function(p, q, fit.weights, show.output, tolPlot, tolConv) {
  if (length(p) != length(q) | 
      length(p) != length(fit.weights) | 
      length(q) != length(fit.weights)) {
    stop(paste("INVALID INPUT, the probability, percentiles and weights",
               "vectors are not of the same length!"), call. = FALSE)
  }
  if (length(p) == 0 | 
      length(q) == 0 | 
      length(fit.weights) == 0) {
    stop(paste("INVALID INPUT, either the vector of probabilities,", 
               "the vector of quantiles or the weights vector is empty!"), 
         call. = FALSE)
  }
  if (!is.numeric(p) | 
      !is.numeric(q) | 
      !is.numeric(fit.weights)) {
    stop("INVALID INPUT, one or more item(s) is (are) not numeric!", 
         call. = FALSE)
  }
  if (any(fit.weights <= 0)) {
    stop(paste("INVALID INPUT, all items of the argument 'fit.weights'", 
               "should be positive!"), call. = FALSE)
  }
  if (min(p) < 0 | max(p) > 1) {
    stop(paste("INVALID INPUT, items of the probability vector should lie",
               "between 0 and 1!"), call. = FALSE)
  }
  if (prod(order(p) == seq_along(p)) == 0 | 
      prod(order(q) == seq_along(q)) == 0) {
    stop(paste("INVALID INPUT, the vector of probabilities/percentiles", 
               "is not ordered!"), call. = FALSE)
  }
  if (!is.logical(show.output)) {
    stop("INVALID INPUT, the argument 'show.output' should be logical!", 
         call. = FALSE)
  }
  if (!is.numeric(tolPlot) | 
      length(tolPlot) != 1 | 
      tolPlot < 0) {
    stop(paste("INVALID INPUT, the argument 'tolPlot' should be a single", 
               "positive numerical value!"), call. = FALSE)
  }
  if (!is.numeric(tolConv) | 
      length(tolConv) != 1 | 
      tolConv < 0) {
    stop(paste("INVALID INPUT, the argument 'tolConv' should be a single", 
               "positive numerical value!"), call. = FALSE)
  }
}

#*******************************************************************************
#*******************************************************************************
# FUNCTIONS FOR FITTING BY PERCENTILES
#*******************************************************************************
#*******************************************************************************


################################################################################
################################################################################
#' This function provides a GUI for choosing a most appropriate continuous
#' distribution for known quantiles.
#'
#' The argument \code{tolPlot} defines a tolerance for plotting graphical diagnostics.
#' If the sums of the differences between the percentiles of the estimated distribution
#' and the given percentiles are smaller than this value, the distribution will be plotted.
#' \cr \cr
#' The items of the probability vector \code{p} should lie between 0 and 1.
#'
#' @name fit.perc
#' @aliases fit.perc
#' @title Choosing distribution by given quantiles
#' @author Matthias Greiner \email{matthias.greiner@@bfr.bund.de} (BfR), \cr
#' Kristin Tolksdorf \email{kristin.tolksdorf@@bfr.bund.de} (BfR), \cr
#' Katharina Schueller \email{schueller@@stat-up.de} (\acronym{STAT-UP} Statistical Consulting), \cr
#' Natalia Belgorodski \email{belgorodski@@stat-up.de} (\acronym{STAT-UP} Statistical Consulting)
#' @usage fit.perc(p = c(0.025, 0.5, 0.975), q = stats::qnorm(p), show.output = FALSE,
#'   tolPlot = 0.1, tolConv = 0.001, fit.weights = rep(1, length(p)))
#' @param p numerical vector of probabilities.
#' @param q numerical vector of quantiles.
#' @param show.output logical, if \code{TRUE} the output of the fitting functions \code{get.distribution.par} will be shown.
#' @param tolPlot single positive numerical value giving a tolerance for plotting graphical diagnostics. 
#'    If the sums of the differences between the distribution percentiles and
#'    the given percentiles are smaller than this value, the distribution will be plotted.
#' @param tolConv positive numerical value, the absolute convergence tolerance for reaching zero.
#' @param fit.weights numerical vector of the same length as a probabilities vector 
#'    \code{p} containing positive values for weighting quantiles. By default all
#'    quantiles will be weighted by 1.
#' @return Returns a named list containing a chosen distribution, its estimated parameters
#' and the data on which the estimation is based.
#' @note This function is used for defining a Monte-Carlo random variate item
#' (\code{mcrv}) in the \code{rrisk} project.
#' @keywords gui
#' @export
#' @examples
#' \dontrun{
#'     chosenDistr1 <- fit.perc()
#'     chosenDistr1
#'     
#'     chosenDistr2 <- fit.perc(tolPlot = 5)
#'     chosenDistr2
#'     
#'     chosenDistr3 <- fit.perc(p = c(0.3, 0.8, 0.9), q = c(10, 20, 40))
#'     chosenDistr3
#'     
#'     chosenDistr4 <- fit.perc(p = c(0.3, 0.8, 0.9), q = c(10, 30, 40))
#'     chosenDistr4
#'     
#'     chosenDistr5 <- fit.perc(p = c(0.3, 0.8, 0.9), q = c(10, 30, 40), tolPlot = 10)
#'     chosenDistr5
#'
#'     ## Fitting a PERT distribution
#'     p <- c(0.025, 0.5, 0.6, 0.975)
#'     q <- round(mc2d::qpert(p = p, min = 0, mode = 3, max = 10, shape = 5), digits = 2)
#'     chosenDistr6 <- fit.perc(p = p, q = q, tolPlot = 10)
#'     chosenDistr6
#' }
fit.perc <- function(p = c(0.025, 0.5, 0.6, 0.975),#c(0.025,0.5, 0.975), 
                     q = round(mc2d::qpert(p = p, min = 0, mode = 3, max = 10, shape = 5), digits = 2),#stats::qnorm(p), 
                     show.output = FALSE, 
                     tolPlot = 0.1, tolConv = 0.001, 
                     fit.weights = rep(1, length(p))) {
  cat("fit.perc\n")
  
  # check consistency of the input data
  assert_fit.perc_input(p, q, fit.weights, show.output, tolPlot, tolConv)

  # load required libraries
  if (class(tcltk::tclRequire("Tktable")) != "tclObj" )
    stop("Tcl package \"Tktable\" required. Please install it.")

  # define help variables for output
  tempEnvir <- new.env()
  tempEnvir$comboDistributions <- c("")
  tempEnvir$chosenD <- NA
  tempEnvir$allParameters <- NA
  tempEnvir$fittedParams <- NA
  
  #---BEGIN: define help variables for tk commands and objects------------------
  pLabel <- "Enter probabilities, sep. by blank"
  qLabel <- "Enter percentiles, sep. by blank"
  wLabel <- "Enter weights, sep. by blank"
  tolPlotLabel <- "Plotting tolerance"
  tolConvLabel <- "Fitting tolerance"
  textWidth <- max(nchar(pLabel), nchar(qLabel), nchar(tolPlotLabel))
  headingFont1 <- tcltk::tkfont.create(size = 12, weight = "bold")
  headingFont2 <- tcltk::tkfont.create(weight = "bold", size = 10)
  pDefault <- round(p, digits = 2)
  qDefault <- round(q, digits = 2)
  wDefault <- round(fit.weights, digits = 2)
  tolPlotDefault <- tolPlot
  tolConvDefault <- tolConv
  p.tclVar <- tcltk::tclVar(pDefault)
  q.tclVar <- tcltk::tclVar(qDefault)
  w.tclVar <- tcltk::tclVar(wDefault)
  tolPlot.tclVar <- tcltk::tclVar(tolPlot)
  tolConv.tclVar <- tcltk::tclVar(tolConv)
  tclarray <- tcltk::tclArray()
  #---END: define help variables for tk commands and objects--------------------
  
  # what happends by pressing "cancel" button
  onCancel <- function(...) {
    #assign("chosenD", value = tcltk::tclvalue(tcltk::tkget(chooseCombobox)), envir = tempEnvir)
    tcltk::tkdestroy(fitpercWindow)
  }
  
  #---BEGIN: what happends by pressing "cancel" button--------------------------
  onOk <- function(...) { # ?
    #fittedParams.temp <- get("allParameters", envir = tempEnvir)
    fittedParams.temp <- tempEnvir$allParameters
    if (!prod(is.na(fittedParams.temp))) { # ?
      #assign("chosenD", 
      #       value = tcltk::tclvalue(tcltk::tkget(chooseCombobox)), 
      #       envir = tempEnvir)
      tempEnvir$chosenD <- tcltk::tclvalue(tcltk::tkget(chooseCombobox))
      #chosenD <- get("chosenD", envir = tempEnvir)
      chosenD <- tempEnvir$chosenD
      if (nchar(chosenD) == 0) # if (chosenD == "")
        chosenD <- "NA" # ?
      else {
        # fittedParams = fP
        fP <- fittedParams.temp[colnames(fittedParams.temp) == chosenD]
        dist_name <- colnames(fP)
        # just the first colum is relevant
        # (there is probably only one column, so as.vector would also work)
        fP <- fP[,1]
        
        # named distribution values (dv)      
        # also setNames(fp, c("mean", "sd")) instead of named c() could
        # work
        # dv <- switch(dist_name,
        #              norm     = c(mean = fP[1], sd = fP[2]),
        #              beta     = c(shape1 = fP[1], shape2 = fP[2]),
        #              cauchy   = c(location = fP[1], scale = fP[2]),
        #              logis    = c(location = fP[1], scale = fP[2]),
        #              t        = c(df = fP[1]),
        #              chisq    = c(df = fP[1]),
        #              chisqnc  = c(df = fP[1], ncp = fP[2]),
        #              exp      = c(rate = fP[1]),
        #              f        = c(df1 = fP[1], df2 = fP[2]),
        #              gamma    = c(shape = fP[1], rate = fP[2]),
        #              lnorm    = c(meanlog = fP[1], sdlog = fP[2]),
        #              unif     = c(min = fP[1], max = fP[2]),
        #              weibull  = c(shape = fP[1], scale = fP[2]),
        #              triang   = c(min = fP[1], mode = fP[2], max = fP[3]),
        #              gompertz = c(shape = fP[1], scale = fP[2]),
        #              pert     = c(min = fP[1], mode  = fP[2], 
        #                           max = fP[3], shape = fP[3]),
        #              tnorm    = c(mean  = fP[1], sd = fP[2], 
        #                           lower = fP[3], upper = fP[4])
        # )
        dv <- switch(dist_name,
                     norm     = setNames(fp, c("mean", "sd")),
                     beta     = setNames(fp, c("shape1", "shape2")),
                     cauchy   = setNames(fp, c("location", "scale")),
                     logis    = setNames(fp, c("location", "scale")),
                     t        = setNames(fp, "df"),
                     chisq    = setNames(fp, "df"),
                     chisqnc  = setNames(fp, c("df", "ncp")),
                     exp      = setNames(fp, "rate"),
                     f        = setNames(fp, c("df1", "df2")),
                     gamma    = setNames(fp, c("shape", "rate")),
                     lnorm    = setNames(fp, c("meanlog", "sdlog")),
                     unif     = setNames(fp, c("min", "max")),
                     weibull  = setNames(fp, c("shape", "scale")),
                     triang   = setNames(fp, c("min", "mode", "max")),
                     gompertz = setNames(fp, c("shape", "scale")),
                     pert     = setNames(c(fp, fp[3]), 
                                         c("min", "mode", "max", "shape")),
                     tnorm    = setNames(fp, c("mean", "sd", "lower", "upper"))
        )
        #assign("fittedParams", 
        #       value = dv, 
        #       envir = tempEnvir)
        tempEnvir$fittedParams <- dv
        #tempEnvir$fittedParams <<- dv # ?
      }
    }
    tcltk::tkdestroy(fitpercWindow)
  }
  #---END: what happends by pressing "cancel" button----------------------------
  
  #-----------------------------------------------------------------------------
  # what happends by pressing "reset" button
  #-----------------------------------------------------------------------------
  onReset <- function(...) {
    p.tclVar <- tcltk::tclVar(pDefault)
    q.tclVar <- tcltk::tclVar(qDefault)
    w.tclVar <- tcltk::tclVar(wDefault)
    tolPlot.tclVar <- tcltk::tclVar(tolPlotDefault)
    tolConv.tclVar <- tcltk::tclVar(tolConvDefault)
    tcltk::tkconfigure(pEntry, text = p.tclVar)
    tcltk::tkconfigure(qEntry, text = q.tclVar)
    tcltk::tkconfigure(wEntry, text = w.tclVar)
    tcltk::tkconfigure(tolPlotEntry, text = tolPlot.tclVar)
    tcltk::tkconfigure(tolConvEntry, text = tolConv.tclVar)
  }  # end of onReset()
  
  #-----------------------------------------------------------------------------
  # what happends by pressing "clear" button
  #-----------------------------------------------------------------------------
  onClear <- function(...) {
    p.tclVar <- tcltk::tclVar("")
    q.tclVar <- tcltk::tclVar("")
    w.tclVar <- tcltk::tclVar("")
    tolPlot.tclVar <- tcltk::tclVar("")
    tolConv.tclVar <- tcltk::tclVar("")
    tcltk::tkconfigure(pEntry, text = p.tclVar)
    tcltk::tkconfigure(qEntry, text = q.tclVar)
    tcltk::tkconfigure(wEntry, text = q.tclVar)
    tcltk::tkconfigure(tolPlotEntry, text = tolPlot.tclVar)
    tcltk::tkconfigure(tolConvEntry, text = tolConv.tclVar)
  } # end of onClear()
  
  #---BEGIN: what happends by pressing "fit" button-----------------------------
  onFit <- function(...) {
    # define help variable for p
    pTemp <- tcltk::tclvalue(tcltk::tkget(pEntry))
    pTemp <- strsplit(pTemp, " ")[[1]]
    pTemp <- c(apply(matrix(pTemp, nrow = 1), 1, 
                     function(x) sub(x, pattern = " ", replacement = "")))
    toRemove <- which(pTemp == "")
    if (length(toRemove > 0)) 
      pTemp <- pTemp[-toRemove]
    p <- as.numeric(pTemp)
    
    # define help variables for q
    qTemp <- tcltk::tclvalue(tcltk::tkget(qEntry))
    qTemp <- strsplit(qTemp, " ")[[1]]
    qTemp <- c(apply(matrix(qTemp, nrow = 1), 1, 
                     function(x) sub(x, pattern = " ", replacement = "")))
    toRemove <- which(qTemp == "")
    if (length(toRemove > 0)) 
      qTemp <- qTemp[-toRemove]
    q <- as.numeric(qTemp)
    
    # define help variables for w(fit.weights)
    wTemp <- tcltk::tclvalue(tcltk::tkget(wEntry))
    wTemp <- strsplit(wTemp, " ")[[1]]
    wTemp <- c(apply(matrix(wTemp, nrow = 1), 1,
                     function(x) sub(x, pattern = " ", replacement = "")))
    toRemove <- which(wTemp == "")
    if (length(toRemove > 0))
      wTemp <- wTemp[-toRemove]
    fit.weights <- as.numeric(wTemp)
    
    # define help variable for tolPlot
    tolPlotTemp <- tcltk::tclvalue(tcltk::tkget(tolPlotEntry))
    tolPlotTemp <- strsplit(tolPlotTemp, " ")[[1]]
    tolPlotTemp <- c(apply(matrix(tolPlotTemp, nrow = 1), 1,
                           function(x) sub(x, pattern = " ", replacement = "")))
    toRemove <- which(tolPlotTemp == "")
    if (length(toRemove > 0)) 
      tolPlotTemp <- tolPlotTemp[-toRemove]
    tolPlot <- as.numeric(tolPlotTemp)
    
    # define help variable for tolConv
    tolConvTemp <- tcltk::tclvalue(tcltk::tkget(tolConvEntry))
    tolConvTemp <- strsplit(tolConvTemp, " ")[[1]]
    tolConvTemp <- c(apply(matrix(tolConvTemp, nrow = 1), 1, 
                           function(x) sub(x, pattern = " ", replacement = "")))
    toRemove <- which(tolConvTemp == "")
    if (length(toRemove > 0)) 
      tolConvTemp <- tolConvTemp[-toRemove]
    tolConv <- as.numeric(tolConvTemp)
    
    # check input data consistency
    if (length(p) == 0) {
      tcltk::tkmessageBox(message = "The inputs are empty! Please correct your input!", icon = "error")
      tcltk::tkfocus(pEntry)
      stop("INVALID INPUT", call. = FALSE)
    }
    if (length(q) == 0) {
      tcltk::tkmessageBox(message = "The inputs are empty! Please correct your input!", icon = "error")
      tcltk::tkfocus(qEntry)
      stop("")
    }
    if (length(fit.weights) == 0) {
      tcltk::tkmessageBox(message = "The inputs are empty! Please correct your input!", icon = "error")
      tcltk::tkfocus(wEntry)
      stop("")
    }
    if (any(is.na(p))) {
      tcltk::tkmessageBox(message = "One or more item(s) is not numeric, please correct your input!", icon = "error")
      tcltk::tkfocus(pEntry)
      stop("INVALID INPUT", call. = FALSE)
    }
    if ( any(is.na(q))) {
      tcltk::tkmessageBox(message = "One or more item(s) is not numeric, please correct your input!", icon = "error")
      tcltk::tkfocus(qEntry)
      stop("INVALID INPUT", call. = FALSE)
    }
    if ( any(is.na(fit.weights))) {
      tcltk::tkmessageBox(message = "One or more item(s) is not numeric, please correct your input!", icon = "error")
      tcltk::tkfocus(wEntry)
      stop("INVALID INPUT", call. = FALSE)
    }
    if ( any((fit.weights) <= 0)) {
      tcltk::tkmessageBox(message = "All weights should be positive, please correct your input!", icon = "error")
      tcltk::tkfocus(wEntry)
      stop("INVALID INPUT", call. = FALSE)
    }
    if (length(p) != length(q) | length(p) != length(fit.weights) | length(q) != length(fit.weights)) {
      tcltk::tkmessageBox(message = "The vectors of percentiles, probabilities and weights are not of the same lengths! Please correct your input!", icon = "error")
      tcltk::tkfocus(pEntry)
      stop("INVALID INPUT", call. = FALSE)
    }
    if (prod(order(p) == seq(1:length(p))) == 0) {
      tcltk::tkmessageBox(message = "The vector of probabilities ist not ordered! Please correct your input!", icon = "error")
      tcltk::tkfocus(pEntry)
      stop("INVALID INPUT", call. = FALSE)
    }
    if (prod(order(q) == seq(1:length(q))) == 0) {
      tcltk::tkmessageBox(message = "The vector of quantiles ist not ordered! Please correct your input!", icon = "error")
      tcltk::tkfocus(qEntry)
      stop("INVALID INPUT", call. = FALSE)
    }
    if (min(p) < 0 | max(p) > 1) {
      tcltk::tkmessageBox(message = "Items of the probability vector should lie between 0 and 1! Please correct your input!", icon = "error")
      tcltk::tkfocus(pEntry)
      stop("INVALID INPUT", call. = FALSE)
    }
    if (!prod(is.numeric(tolPlot)) | length(tolPlot) != 1 | any(tolPlot < 0)) {
      tcltk::tkmessageBox(message = "The tolerance for diagnostic plotting should be a single positive numerical value! Please correct your input!", icon = "error")
      tcltk::tkfocus(tolPlotEntry)
      stop("INVALID INPUT", call. = FALSE)
    }
    if (!prod(is.numeric(tolConv)) | length(tolConv) != 1 | any(tolConv < 0)) {
      tcltk::tkmessageBox(message = "The tolerance for distributions fitting should be a single positive numerical value! Please correct your input!", icon = "error")
      tcltk::tkfocus(tolConvEntry)
      stop("INVALID INPUT", call. = FALSE)
    }
    
    # clear image, table and combobox
    tcltk::tkconfigure(chooseCombobox, values = c(""))
    tcltk::tkset(chooseCombobox, c(""))
    tcltk::tkconfigure(fitResultTable, variable = tcltk::tclArray())
    tkrplot::tkrreplot(imgPlot, hscale = 1.4, vscale = 1.2,
                       fun = function() {
                         graphics::plot(stats::rnorm(20), 
                                        col = "white", 
                                        xlab = "Percentile", ylab = "Percent", 
                                        main = "Graphical diagnostics")
                       }
    )
    
    # calculate results matrix
    fit.results <- rriskFitdist.perc(p, q,
                                     show.output = FALSE, 
                                     tolConv = tolConv, fit.weights)
    
    if (!prod(is.na(fit.results))) { # if res.matrix is not empty
      res.matrix <- fit.results$results
      #assign("allParameters", 
      #       value = res.matrix[1:4, -c(1, 2)], 
      #       envir = tempEnvir)
      tempEnvir$allParameters <- res.matrix[1:4, -c(1, 2)]
      comboDistributions <- colnames(res.matrix)[!apply(res.matrix, 2, 
                                                        function(x) ifelse(all(is.na(x)), TRUE, FALSE))]
      comboDistributions <- comboDistributions[-c(1, 2)]
      #assign("comboDistributions", 
      #       value = comboDistributions, 
      #       envir = tempEnvir)
      tempEnvir$comboDistributions <- comboDistributions
      
      # formatting results
      res.matrix <- as.matrix(apply(res.matrix, c(1, 2), 
                                    function(x) {
                                      if (!is.na(x)) {
                                        if (x > 10000)
                                          x <- "+infty" # ?
                                        else if (x < (-10000))
                                          x <- "-infty" # ?
                                        else 
                                          x <- x
                                      } 
                                    }))
      res.matrix <- cbind(rownames(res.matrix), res.matrix)
      res.matrix <- rbind(colnames(res.matrix), res.matrix)
      res.matrix[1, 1] <- "Percent"
      
      # define data for tktable
      #for (i in 0:(nrow(res.matrix) - 1)) {
      #    for (j in 0:(ncol(res.matrix) - 1)) {
      #        temp <- unlist(res.matrix[i + 1, j + 1])
      #        tclarray[[i, j]] <- temp
      #    }
      #}
      for (i in seq_len(nrow(res.matrix)))
        for (j in seq_len(ncol(res.matrix)))
          tclarray[[i-1, j-1]] <- unlist(res.matrix[i, j])
      
      # changed by LG: cf. comment in method plotDiagnostics.perc() where the message is produced:
      withCallingHandlers(
        tkrplot::tkrreplot(imgPlot,
                           fun = function() plotDiagnostics.perc(fit.results, 
                                                                 tolPlot = tolPlot),
                           hscale = 1.4, vscale = 1.2),
        message = function(m) tcltk::tkmessageBox(message = m$message, icon = "error")
      )
      tcltk::tkconfigure(fitResultTable, variable = tclarray, rows = nrow(res.matrix))
      #tcltk::tkconfigure(chooseCombobox, values = get("comboDistributions", envir = tempEnvir))
      #tcltk::tkset(chooseCombobox, get("comboDistributions", envir = tempEnvir)[1])
      tcltk::tkconfigure(chooseCombobox, values = tempEnvir$comboDistributions)
      tcltk::tkset(chooseCombobox, tempEnvir$comboDistributions[1])
    } else {# if results matrix is empty
      #allParameters.temp <- get("allParameters", envir = tempEnvir)
      allParameters.temp <- tempEnvir$allParameters
      allParameters.temp <- apply(allParameters.temp, c(1, 2), function(x) x <- NA)
      #assign("allParameters", value = allParameters.temp, envir = tempEnvir)
      tempEnvir$allParameters <- allParameters.temp
      #tempEnvir$allParameters <- apply(tempEnvir$allParameters, 
      #c(1, 2), 
      #function(x) x <- NA)
      tcltk::tkraise(fitpercWindow)
      tcltk::tkmessageBox(message = "Sorry, no pdfs found that match with the specified percentiles!", icon = "error")
      tcltk::tkfocus(pEntry)
      stop("no pdfs found that match with the specified percentiles")
    }
    
    #to.remove <- as.matrix(apply(res.mat, 2, function(x) ifelse(all(is.na(x)), TRUE, FALSE)))
    #res.mat <- res.mat[,which(to.remove == FALSE)]
    tcltk::tkraise(fitpercWindow)
  } 
  #---END: onFit----------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # create GUI window and frames
  #-----------------------------------------------------------------------------
  fitpercWindow <- tcltk::tktoplevel(width = 860, height = 580)
  tcltk::tkwm.title(fitpercWindow, "Fitting continuous distributions to given percentiles")
  tcltk::tkwm.resizable(fitpercWindow, FALSE, FALSE)  # fixed size, not resizeable
  #tcltk::tkwm.maxsize(fitpercWindow, 880, 580)
  #tcltk::tkwm.minsize(fitpercWindow, 880, 580)
  allFrame <- tcltk::tkframe(fitpercWindow)
  InputImageFrame <- tcltk::tkframe(allFrame)
  
  #-----------------------------------------------------------------------------
  # create contents of input frame
  #-----------------------------------------------------------------------------
  fitFrame <- tcltk::tkframe(InputImageFrame, relief = "groove", borderwidth = 4)
  inputFrame <- tcltk::tkframe(fitFrame)
  
  tcltk::tkpack(tcltk::tklabel(inputFrame, text = "Input frame", font = headingFont1), pady = c(0, 10))
  tcltk::tkpack(tcltk::tklabel(inputFrame, text = pLabel, width = 30), pady = c(0, 10))
  pEntry <- tcltk::tkentry(inputFrame, width = 30, text = p.tclVar, textvariable = p.tclVar)
  tcltk::tkpack(pEntry, pady = c(0, 10))
  
  tcltk::tkpack(tcltk::tklabel(inputFrame, text = qLabel, width = 30))
  qEntry <- tcltk::tkentry(inputFrame, width = 30, text = q.tclVar, textvariable = q.tclVar)
  tcltk::tkpack(qEntry, pady = c(0, 10))
  
  tcltk::tkpack(tcltk::tklabel(inputFrame, text = wLabel, width = 30))
  wEntry <- tcltk::tkentry(inputFrame, width = 30, text = w.tclVar, textvariable = w.tclVar)
  tcltk::tkpack(wEntry, pady = c(0, 10))
  
  tcltk::tkpack(tcltk::tklabel(inputFrame, text = tolPlotLabel, width = 30))
  tolPlotEntry <- tcltk::tkentry(inputFrame, width = 30, text = tolPlot.tclVar, textvariable = tolPlot.tclVar)
  tcltk::tkpack(tolPlotEntry)
  tcltk::tkpack(inputFrame, side = "top", padx = c(5, 5), pady = c(5, 5))
  tcltk::tkpack(fitFrame)
  
  tcltk::tkpack(tcltk::tklabel(inputFrame, text = tolConvLabel, width = 30))
  tolConvEntry <- tcltk::tkentry(inputFrame, width = 30, text = tolConv.tclVar, textvariable = tolConv.tclVar)
  tcltk::tkpack(tolConvEntry)
  tcltk::tkpack(inputFrame, side = "top", padx = c(5, 5), pady = c(5, 5))
  tcltk::tkpack(fitFrame)
  
  buttonsFrame1 <- tcltk::tkframe(fitFrame)
  fitButton <- tcltk::ttkbutton(buttonsFrame1, width = 8, text = "Fit", command = onFit)
  clearButton <- tcltk::ttkbutton(buttonsFrame1, width = 8, text = "Clear", command = onClear)
  resetButton <- tcltk::ttkbutton(buttonsFrame1, width = 8, text = "Reset", command = onReset)
  tcltk::tkpack(fitButton, side = "left", padx = c(0, 10))
  tcltk::tkpack(clearButton, side = "left", padx = c(0, 10))
  tcltk::tkpack(resetButton, side = "left", padx = c(0, 0))
  tcltk::tkpack(buttonsFrame1, side = "bottom", pady = c(0, 15))
  tcltk::tkpack(fitFrame, side = "right")
  
  #-----------------------------------------------------------------------------
  # create contents of image frame
  #-----------------------------------------------------------------------------
  imgPlot <- tkrplot::tkrplot(InputImageFrame, hscale = 1.4, vscale = 1.2,
                              fun = function() {
                                graphics::plot(stats::rnorm(20), 
                                               col = "white", 
                                               xlab = "Percentile", ylab = "Percent", 
                                               main = "Graphical diagnostics")
                              })
  tcltk::tkpack(imgPlot, side = "left")
  tcltk::tkpack(InputImageFrame)
  
  #-----------------------------------------------------------------------------
  # create table frame
  #-----------------------------------------------------------------------------
  TableFrame <- tcltk::tkframe(allFrame)
  fitResultTable <- tcltk::tkwidget(TableFrame, "table", variable = tcltk::tclArray(),
                                    height = 10, rows = 12, cols = 20, 
                                    background = "white", borderwidth = 2, state = "disabled", 
                                    titlerows = 1, titlecols = 1, 
                                    resizeborders = "none", colwidth = 6, maxwidth = 1200,
                                    yscrollcommand = function(...) tcltk::tkset(yscr, ...), 
                                    selectmode = "extended")
  yscr <- tcltk::tkscrollbar(TableFrame, 
                             command = function(...) tcltk::tkyview(fitResultTable, ...))
  tcltk::tkgrid(fitResultTable, yscr, sticky = "ns")
  tcltk::tkpack(TableFrame)
  
  #-----------------------------------------------------------------------------
  # create buttons and combobox frame
  #-----------------------------------------------------------------------------
  buttonsFrame2 <- tcltk::tkframe(allFrame)
  okButton <- tcltk::ttkbutton(buttonsFrame2, 
                               width = 10, text = "Ok",
                               command = onOk)
  cancelButton <- tcltk::ttkbutton(buttonsFrame2, width = 10, text = "Cancel", 
                                   command = onCancel)
  #chooseCombobox <- tcltk::ttkcombobox(buttonsFrame2, 
  #                                     values = get("comboDistributions", 
  #                                                  envir = tempEnvir), 
  #                                     width = 10, state = "readonly")
  chooseCombobox <- tcltk::ttkcombobox(buttonsFrame2, 
                                       values = tempEnvir$comboDistributions,
                                       width = 10, state = "readonly")
  tcltk::tkpack(tcltk::tklabel(buttonsFrame2, 
                               text = "chosen distribution", 
                               font = headingFont2, width = 15), 
                side = "left", padx = c(0, 15))
  tcltk::tkpack(chooseCombobox, side = "left", padx = c(0, 200))
  tcltk::tkpack(okButton, side = "left", padx = c(0, 15))
  tcltk::tkpack(cancelButton, side = "left", padx = c(0, 15))
  tcltk::tkpack(buttonsFrame2, pady = c(10, 0))
  
  #-----------------------------------------------------------------------------
  # place allFrame on GUI window
  #-----------------------------------------------------------------------------
  tcltk::tkpack(allFrame, padx = c(15, 15), pady = c(0, 10))
  
  #-----------------------------------------------------------------------------
  # create matrix with fitting results
  #-----------------------------------------------------------------------------
  fit.results <- rriskFitdist.perc(pDefault, qDefault, 
                                   show.output = FALSE, tolConv = tolConv, 
                                   fit.weights = wDefault)
  
  #-----------------------------------------------------------------------------
  # fill image and table with fitting results
  #-----------------------------------------------------------------------------
  if (!prod(is.na(fit.results))) { # if res.matrix is not empty
    res.matrix <- fit.results$results
    #assign("allParameters", value = res.matrix[1:4,-c(1, 2)], envir = tempEnvir)
    tempEnvir$allParameters <- res.matrix[1:4,-c(1, 2)]
    comboDistributions <- colnames(res.matrix)[!apply(res.matrix, 2, 
                                                      function(x) ifelse(all(is.na(x)), TRUE, FALSE))]
    comboDistributions <- comboDistributions[-c(1, 2)]
    #assign("comboDistributions", value = comboDistributions, envir = tempEnvir)
    tempEnvir$comboDistributions <- comboDistributions
    #tcltk::tkconfigure(chooseCombobox, values = get("comboDistributions", envir = tempEnvir))
    #tcltk::tkset(chooseCombobox, get("comboDistributions", envir = tempEnvir)[1])
    tcltk::tkconfigure(chooseCombobox, values = tempEnvir$comboDistributions)
    tcltk::tkset(chooseCombobox, tempEnvir$comboDistributions[1])
    #-----------------------------------------------------------------------------
    # formatting results
    #-----------------------------------------------------------------------------
    res.matrix <- as.matrix(apply(res.matrix, c(1, 2),
                                  function(x) {
                                    if (!is.na(x)) {
                                      if (x > 10000)
                                        x <- "+infty"
                                      else if (x < (-10000))
                                        x <- "-infty"
                                      else 
                                        x <- x
                                    } 
                                  }))
    res.matrix <- cbind(rownames(res.matrix), res.matrix)
    res.matrix <- rbind(colnames(res.matrix), res.matrix)
    res.matrix[1, 1] <- "Percent"
    tableRows <- nrow(res.matrix)
    
    #-----------------------------------------------------------------------------
    # create data (tclarray) for tktable
    #-----------------------------------------------------------------------------
    #for (i in 0:(nrow(res.matrix) - 1)) {
    #    for (j in 0:(ncol(res.matrix) - 1)) {
    #        temp <- unlist(res.matrix[i + 1, j + 1])
    #        tclarray[[i, j]] <- temp
    #    }
    #}
    for (i in seq_len(nrow(res.matrix)))
      for (j in seq_len(ncol(res.matrix)))
        tclarray[[i-1, j-1]] <- unlist(res.matrix[i, j])
    
    # changed by LG: cf. comment in method plotDiagnostics.perc() where the message is produced:
    withCallingHandlers(
      tkrplot::tkrreplot(
        imgPlot,
        fun = function() plotDiagnostics.perc(fit.results, tolPlot = tolPlot),
        hscale = 1.4, vscale = 1.2),
      message = function(m) tcltk::tkmessageBox(message = m$message, icon = "error")
    )
    tcltk::tkconfigure(fitResultTable, variable = tclarray, rows = tableRows)
  } else {
    tcltk::tkmessageBox(
      message = "Sorry, no pdfs found that match with the specified percentiles!", 
      icon = "error")
    tcltk::tkfocus(pEntry)
  }
  
  tcltk::tkfocus(fitpercWindow)
  tcltk::tkwait.window(fitpercWindow)
  
  #-----------------------------------------------------------------------------
  # generate output
  #-----------------------------------------------------------------------------
  #chosenD <- get("chosenD", envir = tempEnvir)
  #fittedParams <- get("fittedParams", envir = tempEnvir)
  chosenD <- tempEnvir$chosenD
  fittedParams <- tempEnvir$fittedParams
  output <- list(data.frame(p, q), chosenD, fittedParams)
  names(output) <- c("p/q", "chosenDistr", "fittedParams")
  
  #-----------------------------------------------------------------------------
  # output message
  #-----------------------------------------------------------------------------
  print.on.exit <- function(chosenD) {
    exitMessage <- switch(chosenD,
                          norm     = "Normal (norm)",
                          beta     = "Beta (beta)",
                          cauchy   = "Cauchy (cauchy)",
                          logis    = "Logistic (logis)",
                          t        = "Student's t (t)",
                          chisq    = "Chi-squared (chisq)",
                          chisqnc  = "Non-central chi-squared (chisqnc)",
                          exp      = "Exponential (exp)",
                          f        = "F (f)",
                          gamma    = "Gamma (gamma)",
                          lnorm    = "Log-normal (lnorm)",
                          unif     = "Uniform (unif)",
                          weibull  = "Weibull (weibull)",
                          triang   = "Triangular (triang)",
                          gompertz = "Gompertz (gompertz)",
                          pert     = "Beta-pert (pert)",
                          tnorm    = "Truncated normal (tnorm)")
    cat("Chosen continuous distribution is:", exitMessage)
    cat("\nFitted parameters are: \n")
    print(fittedParams)
  }
  #-----------------------------------------------------------------------------
  # output
  #-----------------------------------------------------------------------------
  on.exit(print.on.exit(chosenD)) # ?
  return(invisible(output))
} # end of function fit.perc()