#' @name validateSimulationForm
#' @title validateSimulationForm
#' @description Validates scoring modal and provides direct feedback to user.
#' @param input Contains UI input objects.
#' @return Boolean value (valid / invalid).
#' @export
validateSimulationForm <- function(input) {
  cat("validateSimulationForm\n")
  checkRules(inputId = "txbN",
             value   = input$txbN,
             rules   = list("required",
                            "isInteger",
                            list("smallerThan", 100001),
                            list("greaterThan", 0))
  ) &
  checkRules(inputId = "txbN2d",
             value   = input$txbN2d,
             rules   = list("required",
                            "isInteger",
                            list("smallerThan", 501),
                            list("greaterThan", 0))
  ) &
  checkRules(inputId = "txbAbserrors",
             value   = input$txbAbserrors,
             rules   = list("required",
                            "isNumeric",
                            list("smallerThan", 1),
                            list("greaterThan", 0),
                            list("limit", 32))
  )
}
