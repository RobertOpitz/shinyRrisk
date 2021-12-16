#' @name displayUncertainties
#' @title displayUncertainties
#' @description Updates UI of uncertainties context.
#' @param output Contains UI output objects.
#' @param ns Namespace.
#' @return NULL
#' @export
displayUncertainties <- function(output, ns) {
  cat("displayUncertainties\n")

  session <- shiny::getDefaultReactiveDomain()
  model   <- session$userData$model()
  
  if (length(model@uncertainties@uncertainties) > 0)
    updateSelectInput(session  = session,
                      inputId  = "slbMaincategory",
                      selected = model@uncertainties@uncertainties[[1]]@namemain)

  updateTextAreaInput(session = session,
                      inputId = "tbaNote",
                      value   = model@uncertainties@note)

  df_uncertainties <- toDataFrame(model@uncertainties)

  uncert_categories <- c("modelling approach uncertainties",
                         "scenario uncertainties",
                         "other uncertainties")
  table_ids <- c("tblModelUncertCategories",
                 "tblSceneUncertCategories",
                 "tblOtherUncertCategories")
  
  mapply(function(uc, tbl_id, df_uncertainties) {
    # subset data frame of uncertainties according to uncert categories
    this_df <- subset(df_uncertainties, 
                      subset = namemain == uc)
    # reset row numbers
    rownames(this_df) <- NULL
    # add data frame as interactive table to output
    output[[ tbl_id ]] <- DT::renderDT({
      if (is.null(this_df))
        return(NULL)
      
      addActionButtonsColumn(this_df,
                             c("Assessment of", "Sub Category", 
                               "Explanation", "Actions"),
                             c('edit', 'delete'),
                             ns)
    })
    output
  }, uncert_categories, table_ids, 
  MoreArgs = list(df_uncertainties = df_uncertainties))
}
