#' @name getScoringMeaningsDetailed
#' @title getScoringMeaningsDetailed
#' @description Builds a vector containing possible scoring meanings.
#' @return Vector of scoring meanings.
#' @export
getScoringMeaningsDetailed <- function() {
  cat("getScoringMeaningsDetailed\n")
  c("not applicable" = 1,
    "low 0"          = 2,
    "low -"          = 3,
    "low -/+"        = 4,
    "low +"          = 5,
    "medium --"      = 6,
    "medium --/++"   = 7,
    "medium ++"      = 8,
    "high ---"       = 9,
    "high ---/+++"   = 10,
    "high +++"       = 11,
    "high ?-"        = 12,
    "high ?-/+"      = 13,
    "high ?+"        = 14)
}

