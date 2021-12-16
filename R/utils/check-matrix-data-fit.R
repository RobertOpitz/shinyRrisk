#' @name checkMatrixDataFit
#' @title checkMatrixDataFit
#' @description Checks for all scores of uncertainties to have the same length and if this length fits those of scoring system.
#' @param modelUncertainties List of uncertainties.
#' @param scoring Scoring system.
#' @return Boolean.
#' @export
checkMatrixDataFit <- function(modelUncertainties, modelSsystem) {
  cat("checkMatrixDataFit\n")

  lengthsFit <- TRUE

  scoreTargetLength <- length(modelUncertainties[[1]]@scores)
  
  if (scoreTargetLength != length(modelSsystem@scoring))
    lengthsFit <- FALSE

  for (uncert in modelUncertainties)
    if (length(uncert@scores) != scoreTargetLength) {
      lengthsFit <- FALSE
      break
    }
  
  lengthsFit
}
