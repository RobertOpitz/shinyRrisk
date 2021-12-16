#' @name addActionButtonsColumn
#' @title addActionButtonsColumn
#' @description Adds an action column to a table object for interacting with displayed data.
#' @param df Table object.
#' @param colnames New column names for the whole table object.
#' @param actions Buttons to be displayed within the actions column. Possible values: scoring, edit, delete.
#' @param ns Namespace.
#' @return HTML source code.
#' @export
addActionButtonsColumn <- function(df, colnames, actions, ns) {
  cat("addActionButtonsColumn\n")

  actionButtons <- unlist(lapply(seq_len(nrow(df)), 
                                 buildActionButtons, 
                                 actions = actions, ns = ns))

  DT::datatable(cbind(df, actions = actionButtons),
                # Need to disable escaping for html as string to work
                escape    = FALSE,
                colnames  = colnames,
                selection = 'single',
                options   = list(pageLength = 8))
}
