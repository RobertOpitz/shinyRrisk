#' @name buildDfAuthors
#' @title buildDfAuthors
#' @description Builds a data frame from an author list.
#' @param authors List of authors.
#' @return Dataframe of authors.
#' @export
buildDfAuthors <- function(authors) {
  cat("buildDfAuthors\n")
  
  df <- data.frame()
  for (author in authors)
    df <- rbind.data.frame(df,
                           data.frame(Name        = author@name,
                                      Institution = author@institution,
                                      Email       = author@email))
  
  df
}
