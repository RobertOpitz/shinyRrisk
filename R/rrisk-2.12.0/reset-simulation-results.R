#' @name resetSimulationResults
#' @aliases resetSimulationResults
#' @title Non-executable auxiliary function
#' @usage resetSimulationResults(rriskModel)
#' @param rriskModel ...
#' @keywords run
#' @export

resetSimulationResults <- function(rriskModel) {
  
  cat("\nReset simulation results...\n\n")
  
  rriskModel@output@fullout.1d  <- list()
  rriskModel@output@relaxout.1d <- list()
  #rriskModel@output@runtime1d   <- NULL
  rriskModel@output@fullout.2d  <- c()
  rriskModel@output@uncitems.2d <- c()
  rriskModel@output@OFname.2d   <- ""
  rriskModel@output@summaries   <- c()
  rriskModel@output@OFcdfCI     <- c()
  #rriskModel@output@runtime2d   <- NULL
  
  rriskModel
}