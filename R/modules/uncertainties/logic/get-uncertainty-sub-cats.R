#' @name getUncertaintySubCats
#' @title getUncertaintySubCats
#' @description Creates a vector containing names of uncertainty subcategories.
#' @return Vector of uncertainty subcategories.
#' @export
getUncertaintySubCats <- function() {
  cat("getUncertaintySubCats\n")
  session       <- shiny::getDefaultReactiveDomain()
  uncertainties <- session$userData$model()@uncertainties@uncertainties
  
  sapply(uncertainties, function(x) x@namesub)
}
