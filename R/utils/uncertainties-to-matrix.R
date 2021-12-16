#' @description Function that displays the uncertaintiy information in a matrix
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name uncertainties2matrix
#' @aliases uncertainties2matrix
#' @title Function that displays the uncertaintiy information in a matrix
#' @usage uncertainties2matrix(modelUncertainties,modelSsystem)
#' @param modelUncertainties the slot unvertainties@@uncertainties of an instance of the \code{modelClass} 
#' @param modelSsystem the slot scoring of an instance of the \code{modelClass} 
#' @keywords misc
#' @export
#' @examples

uncertainties2matrix <- function(modelUncertainties, modelSsystem) {
  cat("uncertainties2matrix\n")
  
  # get uncertainty matrix
  uncertMatrix <- NULL
  for (this in modelUncertainties)
    uncertMatrix <- rbind(uncertMatrix, this@scores)
  
  # add colnames
  colnames(uncertMatrix) <- sapply(modelSsystem@scoring,
                                   function(scoring) paste0("(", scoring@notation, ")"))
  
  # add rownames
  rownames(uncertMatrix) <- sapply(modelUncertainties,
                                   function(modelUncertainty) modelUncertainty@namesub)
  
  uncertMatrix
}