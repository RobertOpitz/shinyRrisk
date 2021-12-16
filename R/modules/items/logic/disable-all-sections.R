#' @name disableAllSections
#' @title disableAllSections
#' @description Hides all sections from items modal.
#' @return NULL
#' @export
disableAllSections <- function() {
  cat("disableAllSections\n")
  shinyjs::hide("divItemProbabilityDensity")
  shinyjs::hide("divItemDiscrete")
  shinyjs::hide("divItemData")
  shinyjs::hide("divItemMcrv")
  shinyjs::hide("divItemNumvFnrv")
  shinyjs::hide("divItemRsrv")
  shinyjs::hide("divItemBsrv")
}
