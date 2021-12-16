#' @name matrixAddSpan
#' @title matrixAddSpan
#' @description Adds color span to result matrix in fitting tab.
#' @param result Result matrix.
#' @return Dataframe of result matrix.
#' @export
matrixAddSpan <- function(result) {
  cat("matrixAddSpan\n")
  
  for (i in seq_len(nrow(result))) {
    if (result[i, 8] == "not rejected")
      result[i, 8] <- paste0("<span class='not-rejected'>", 
                             result[i, 8],
                             "</span>")
    else
      result[i, 8] <- paste0("<span class='rejected'>",
                             result[i, 8], 
                             "</span>")

    if (result[i, 10] == "not rejected")
      result[i, 10] <- paste0("<span class='not-rejected'>", 
                              result[i, 10], 
                              "</span>")
    else
      result[i, 10] <- paste0("<span class='rejected'>", 
                              result[i, 10], 
                              "</span>")
  }

  result
}
