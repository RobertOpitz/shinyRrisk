#' @name getItemToolTips
#' @title getItemToolTips
#' @description Builds a list containing item tool tips.
#' @return Item tool tip.
#' @export
getItemToolTips <- function(field) {
  cat("getItemToolTips\n")
  switch(field,
         "type"        = "Refer to annex B for an explanation of each item type",
         "scores"      = "A space–separated list of score values (ranging according to a scoring system (for example from 0 to 3), one for each assessed criterion of uncertainty. See userguide chapter scoring system",
         "unit"        = "Measurement unit and (optional) unit of observation",
         "assumptions" = "This field contains the assumptions underlying the item",
         "reference"   = "Provide one or more bibliographic references. BibTeX–style is supported"
  )
}
