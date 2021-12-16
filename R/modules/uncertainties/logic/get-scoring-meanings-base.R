#' @name getScoringMeaningsBasic
#' @title getScoringMeaningsBasic
#' @description Builds a vector containing basic scoring meanings.
#' @return Vector of scoring meanings.
#' @export
getScoringMeaningsBasic <- function() {
  cat("getScoringMeaningBasic\n")
  c("not applicable" = 1,
    "low"            = 2,
    "medium"         = 3,
    "high"           = 4)
}
