#' @name updateProbabilityDensitySection
#' @title updateProbabilityDensitySection
#' @description Updates item modals to react to input changes.
#' @param input Contains UI input objects.
#' @return NULL
#' @export
updateProbabilityDensitySection <- function(input) {
  cat("updateProbabilityDensitySection\n")
  session <- shiny::getDefaultReactiveDomain()
  
  if (strtoi(input$txbRowNum) > 0) {
    itemsExt <- session$userData$itemsExt()
    itemExt  <- itemsExt@items[[strtoi(input$txbRowNum)]]
  } else
    itemExt <- new(Class = "itemClassExt", name = "Dummy")

  shinyjs::hide("divItemMcrv")
  shinyjs::hide("divItemDiscrete")

  for (i in 1:4)
    hideFeedback(inputId = paste0("txbValue", i), 
                 session = session)

  hideTab(inputId = "tabItem", 
          target  = "Fitting", 
          session = getDefaultReactiveDomain())

  if (input$slbProbabilityDensity == "discrete")
    shinyjs::show("divItemDiscrete")
  else if (input$slbProbabilityDensity == "fitting") {
    showTab(inputId = "tabItem", 
            target  = "Fitting", 
            session = getDefaultReactiveDomain())
  } else if (input$slbProbabilityDensity != "") {

    shinyjs::show("divItemMcrv")

    prob_dens_vals <- getProbabilityDensityValues(input$slbProbabilityDensity)
    n <- length(prob_dens_vals)

    for (i in 1:4) {
      input_id <- paste0("txbValue", i)
      if (i > n)
        shinyjs::hide(input_id)
      else {
        shinyjs::show(input_id)
        updateTextInput(session = session, 
                        inputId = input_id, 
                        label   = paste(prob_dens_vals[i], "*"), 
                        value   = slot(itemExt, paste0("value", i)))
      }
    }
  }
}
