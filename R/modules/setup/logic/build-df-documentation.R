#' @name buildDfDocumentation
#' @title buildDfDocumentation
#' @description Builds a data frame from an documentation object.
#' @param basics Documentation object.
#' @return Dataframe of documentation.
#' @export
buildDfDocumentation <- function(basics) {
  cat("buildDfDocumentation\n")
  data.frame(Name = c("Basic Description", "Background", "Objectives", "Scope", 
                      "Description"),
             Explanation = sapply(basics, function(explanation) explanation))
}
