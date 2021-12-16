#' @name getUncertaintyTypes
#' @title getUncertaintyTypes
#' @description Builds a vector containing possible uncertainty types.
#' @return Vector of uncertainty types.
#' @export
getUncertaintyTypes <- function() {
  cat("getUncertaintyTypes\n")
  list("modelling approach uncertainties" = 1,
       "scenario uncertainties"           = 2,
       "other uncertainties"              = 3)
  #switch(uncertType,
  #       "modelling approach uncertainties" = 1,
  #       "scenario uncertainties"           = 2,
  #       "other uncertainties"              = 3)
}

# getUncertaintyNames <- function() {
#   cat("getUncertaintyNames\n")
# 
#   c("modelling approach uncertainties",
#     "scenario uncertainties",
#     "other uncertainties")
# }
