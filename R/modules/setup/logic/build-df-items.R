#' @name buildDfItems
#' @title buildDfItems
#' @description Builds a data frame from an item list.
#' @param items List of items.
#' @return Dataframe of items.
#' @export
buildDfItems <- function(items) {
  cat("buildDfItems\n")
  df <- data.frame()
  for (item in items)
    df <- rbind.data.frame(df,
                           data.frame(Name        = item@name,
                                      Description = item@title,
                                      Type        = item@type,
                                      Definition  = item@definition,
                                      Dependency  = item@depitem,
                                      Unit        = item@unit,
                                      Role        = item@role,
                                      Assumptions = item@assumptions,
                                      Reference   = item@reference,
                                      Command     = item@fullc))
  
  df
}
