#' @description This function provides a GUI for choosing a most appropriate continuous
#' distribution fitted to given data.
#'
#' @name fit.cont
#' @aliases fit.cont
#' @title GUI for fitting continuous distributions to given data
#' @author Matthias Greiner \email{matthias.greiner@@bfr.bund.de} (BfR), \cr
#' Kristin Tolksdorf \email{kristin.tolksdorf@@bfr.bund.de} (BfR), \cr
#' Katharina Schueller \email{schueller@@stat-up.de} (\acronym{STAT-UP} Statistical Consulting), \cr
#' Natalia Belgorodski \email{belgorodski@@stat-up.de} (\acronym{STAT-UP} Statistical Consulting)
#' @usage fit.cont(data2fit)
#' @param data2fit numerical vector, data to be fitted.
#' @return Returns chosen continuous distribution, estimated parameters and data,
#' on which the fitting is based.
#' @note This function is used for defining a Monte-Carlo random variate item
#' (\code{mcrv}) in the \code{rrisk} project.
#' @keywords gui
#' @export
#' @importFrom eha rgompertz
#' @importFrom eha dgompertz
#' @importFrom eha pgompertz
#' @importFrom eha qgompertz
#' @importFrom mc2d rtriang
#' @importFrom mc2d dtriang
#' @importFrom mc2d ptriang
#' @importFrom mc2d qtriang
#' @examples
#' \dontrun{
#'   if ( class(tcltk::tclRequire("Tktable")) == "tclObj" ) {
#'     res1 <- fit.cont(data2fit = rgamma(374, 4, 0.08))
#'     res1
#'
#'     res2 <- fit.cont(data2fit = rbeta(300, shape1 = 1, shape2 = 2))
#'     res2
#'
#'     res3 <- fit.cont(data2fit = mc2d::rtriang(300, min = 1, mode = 3, max = 10))
#'     res3
#'
#'     res4 <- fit.cont(data2fit = stats::rnorm(300))
#'     res4
#'   }
#' }
#' 
fit.cont <- function(data2fit = stats::rnorm(1000), continuousFunction) {
  cat("fit.cont\n")
  
  # checking input
  if (!is.null(dim(data2fit)))
    stop("Argument 'data2fit' should be a simple numerical vector!", 
         call. = FALSE)

  if (!all(is.numeric(data2fit)))
    stop("One or more items of 'data2fit' is/are not numeric!", 
         call. = FALSE)
  
  if (any(is.na(data2fit)))
    stop("'data2fit' contains missing values, fitting procedure is not possible!", 
         call. = FALSE)
  
  # create fitting results matrix
  useFitdist.results <- useFitdist(data2fit)
  
  if (is.null(useFitdist.results)) {
    on.exit(return(invisible(NULL)))
    stop("exit fit.cont() and return NA's", call. = FALSE)
  }
  
  res.matrix <- useFitdist.results$res.matrix
  fit.list <- useFitdist.results$fit.list
  
  notRejectedIndex <- which(res.matrix[, which(colnames(res.matrix) == "H(KS)")] == "not rejected")
  
  newIndizes <- c(notRejectedIndex, setdiff(1:nrow(res.matrix), notRejectedIndex))
  res.matrix <- res.matrix[newIndizes, ]

  # create tcltk::tclArray for results matrix
  res.matrix <- cbind(dimnames(res.matrix)[[1]], res.matrix)
  
  shiny::getDefaultReactiveDomain()$userData$resultMatrix(res.matrix)
  
  res.matrix <- rbind(c(dimnames(res.matrix)[[2]]), res.matrix)
  res.matrix[1, 1] <- "Family"
  
  #-----------------------------------------------------------------------------
  # create temporer environment
  #-----------------------------------------------------------------------------
  #assign("tempEnvir", value = new.env())
  #assign("chosenD", value = continuousFunction, envir = tempEnvir)
  #assign("fittedParams", value = fit.list[[which(names(fit.list) == continuousFunction)]]$estimate, envir = tempEnvir)
  # environment only useful for the very old tcltk code, which is not used
  tempEnvir <- new.env()
  tempEnvir$chosenD <- continuousFunction
  tempEnvir$fittedParams <- fit.list[[which(names(fit.list) == continuousFunction)]]$estimate
  
  #-----------------------------------------------------------------------------
  # function for apdating diagnoctic plot
  #-----------------------------------------------------------------------------
  #updatePlot <- function(...) {
  #}
  
  #-----------------------------------------------------------------------------
  # function for plotting fitting diagnostics
  #-----------------------------------------------------------------------------
  plotDiagnostics <- function(...) {
    distr <- continuousFunction
    
    if (distr == "Normal") distname = "norm"
    if (distr == "Beta") distname = "beta"
    if (distr == "Cauchy") distname = "cauchy"
    if (distr == "Logistic") distname = "logis"
    if (distr == "Exponential") distname = "exp"
    if (distr == "Chi-square") distname = "chisq"
    if (distr == "Uniform") distname = "unif"
    if (distr == "Gamma") distname = "gamma"
    if (distr == "Lognormal") distname = "lnorm"
    if (distr == "Weibull") distname = "weibull"
    if (distr == "Student") distname = "t"
    if (distr == "F") distname = "f"
    if (distr == "Gompertz") distname = "gompertz"
    if (distr == "Triangular") distname = "triang"
    
    params <- fit.list[[which(names(fit.list) == distr)]]$estimate
    x <- seq(min(data2fit) - (max(data2fit) - min(data2fit)) * 0.05,
             max(data2fit) + (max(data2fit) - min(data2fit)) * 0.05, 
             length = length(data2fit))
    
    rdistname <- paste("r", distname, sep = "")
    ddistname <- paste("d", distname, sep = "")
    pdistname <- paste("p", distname, sep = "")
    qdistname <- paste("q", distname, sep = "")
    
    set.seed(1)
    y <- do.call(rdistname, c(list(n = 300), as.list(params)))
    d <- do.call(ddistname, c(list(x = x), as.list(params)))
    p <- do.call(pdistname, c(list(q = data2fit), as.list(params)))
    pp <- do.call(pdistname, c(list(q = x), as.list(params)))
    
    shiny::getDefaultReactiveDomain()$userData$pltEmpTheor(
      list(data2fit = data2fit, d = d, x = x)
    )
    
    shiny::getDefaultReactiveDomain()$userData$pltQq(
      list(data2fit = data2fit, y = y)
    )
    
    shiny::getDefaultReactiveDomain()$userData$pltCdf2(
      list(data2fit = data2fit, x = x, pp = pp)
    )
    
    
    shiny::getDefaultReactiveDomain()$userData$pltPp(
      list(data2fit = data2fit, p = p)
    )
    
  } # end of function plot.diagnostics()
  
  #-----------------------------------------------------------------------------
  # what to do on "Ok" button
  #-----------------------------------------------------------------------------
  #onOK <- function(...) {
  #  assign("chosenD", value = tcltk::tclvalue(rbValue), envir = tempEnvir)
  #  fittedParams <- fit.list[[which(names(fit.list) == tcltk::tclvalue(rbValue))]]$estimate
  #  assign("fittedParams", value = fittedParams, envir = tempEnvir)
  #  tcltk::tkdestroy(fitContDistWindow)
  #} # end of onOk()
  
  #-----------------------------------------------------------------------------
  # what to do on "Cancel" button
  #-----------------------------------------------------------------------------
  #onCancel <- function(...) {
  #  tcltk::tkdestroy(fitContDistWindow)
  #} # end of onCancel()
  

  #index <- 0 # dead code

  plotDiagnostics()

  # # all the if statements are dead code
  # if (is.element("Normal", dimnames(res.matrix)[[1]]))
  #   index <- which(rownames(res.matrix) == "Normal") - 1
  # 
  # if (is.element("Beta", dimnames(res.matrix)[[1]]))
  #   index <- which(rownames(res.matrix) == "Beta") - 1
  # 
  # if (is.element("Cauchy", dimnames(res.matrix)[[1]]))
  #   index <- which(rownames(res.matrix) == "Cauchy") - 1
  # 
  # if (is.element("Logistic", dimnames(res.matrix)[[1]]))
  #   index <- which(rownames(res.matrix) == "Logistic") - 1
  # 
  # if (is.element("Exponential", dimnames(res.matrix)[[1]]))
  #   index <- which(rownames(res.matrix) == "Exponential") - 1
  # 
  # if (is.element("Chi-square", dimnames(res.matrix)[[1]]))
  #   index <- which(rownames(res.matrix) == "Chi-square") - 1
  # 
  # if (is.element("Uniform", dimnames(res.matrix)[[1]]))
  #   index <- which(rownames(res.matrix) == "Uniform") - 1
  # 
  # if (is.element("Gamma", dimnames(res.matrix)[[1]]))
  #   index <- which(rownames(res.matrix) == "Gamma") - 1
  # 
  # if (is.element("Lognormal", dimnames(res.matrix)[[1]]))
  #   index <- which(rownames(res.matrix) == "Lognormal") - 1
  # 
  # if (is.element("Weibull", dimnames(res.matrix)[[1]]))
  #   index <- which(rownames(res.matrix) == "Weibull") - 1
  # 
  # if (is.element("F", dimnames(res.matrix)[[1]]))
  #   index <- which(rownames(res.matrix) == "F") - 1
  # 
  # if (is.element("Student", dimnames(res.matrix)[[1]]))
  #   index <- which(rownames(res.matrix) == "Student") - 1
  # 
  # if (is.element("Gompertz", dimnames(res.matrix)[[1]]))
  #   index <- which(rownames(res.matrix) == "Gompertz") - 1
  # 
  # if (is.element("Triangular", dimnames(res.matrix)[[1]]))
  #   index <- which(rownames(res.matrix) == "Triangular") - 1

  # generate output
  #chosenD <- get("chosenD", envir = tempEnvir)
  #fittedParams <- get("fittedParams", envir = tempEnvir)
  chosenD      <- tempEnvir$chosenD
  fittedParams <- tempEnvir$fittedParams
  exitMessage  <- "NA"
  
  if (!is.na(chosenD)) {
    if (chosenD == "Triangular") {
      chosenD <- "triang"
      exitMessage <- "Triangular (triang)"
    } else if (chosenD == "Gompertz") {
      chosenD <- "gompertz"
      exitMessage <- "Gompertz (gompertz)"
    } else if (chosenD == "Student") {
      chosenD <- "t"
      exitMessage <- "Student's t (t)"
    } else if (chosenD == "F") {
      chosenD <- "f"
      exitMessage <- "F (f)"
    } else if (chosenD == "Weibull") {
      chosenD <- "weibull"
      exitMessage <- "Weibull (weibull)"
    } else if (chosenD == "Lognormal") {
      chosenD <- "lnorm"
      exitMessage <- "Log-normal (lnorm)"
    } else if (chosenD == "Gamma") {
      chosenD <- "gamma"
      exitMessage <- "Gamma (gamma)"
    } else if (chosenD == "Uniform") {
      chosenD <- "unif"
      exitMessage <- "Uniform (unif)"
    } else if (chosenD == "Chi-square") {
      chosenD <- "chisq"
      exitMessage <- "Chi-squared (chisq)"
    } else if (chosenD == "Exponential") {
      chosenD <- "exp"
      exitMessage <- "Exponential (exp)"
    } else if (chosenD == "Logistic") {
      chosenD <- "logis"
      exitMessage <- "Logistic (logis)"
    } else if (chosenD == "Cauchy") {
      chosenD <- "cauchy"
      exitMessage <- "Cauchy (cauchy)"
    } else if (chosenD == "Beta") {
      chosenD <- "beta"
      exitMessage <- "Beta (beta)"
    } else if (chosenD == "Normal") {
      chosenD <- "norm"
      exitMessage <- "Normal (norm)"
    }
  }
  
  output <- list(data2fit, chosenD, fittedParams, res.matrix)
  names(output) <- c("data2fit", "chosenDistr", "fittedParams", "resultMatrix")
  
  #print.on.exit <- function(chosenD) {
  #  cat(paste("\nChosen continuous distribution is:", exitMessage))
  #  cat("\nFitted parameters are: \n")
  #  print(fittedParams)
  #  cat("\n")
  #} # end of fucntion print.on.exit()
  
  #on.exit(print.on.exit(chosenD))
  
  # return(list(item = invisible(output), matrix = res.matrix))
  return(invisible(output))
}