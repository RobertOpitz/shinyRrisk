#' @name getScoringValues
#' @title getScoringValues
#' @description Builds a vector containing possible scoring values.
#' @return Vector of item names.
#' @export
getScoringValues <- function() {
  cat("getScoringValues\n")
  c("1: not applicable" = 1,
    "2: low 0 to low 0" = 2,
    "3: low 0 to low -" = 3,
    "4: low 0 to low -/+" = 4,
    "5: low 0 to low +" = 5,
    "6: low 0 to medium --" = 6,
    "7: low 0 to medium --/++" = 7,
    "8: low 0 to medium ++" = 8,
    "9: low 0 to high ---" = 9,
    "10: low 0 to high ---/+++" = 10,
    "11: low 0 to high +++" = 11,
    "12: low 0 to high ?-" = 12,
    "13: low 0 to high ?-/+" = 13,
    "14: low 0 to high ?+" = 14
  )
}
