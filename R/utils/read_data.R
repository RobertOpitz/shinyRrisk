read_data <- function(file_name) {

  result <- tryCatch(expr = {read.csv(file_name, 
                                      header = TRUE, 
                                      sep = ",")}, # changed from ; to , 
                     error = function(error) return(error))

  if (inherits(result, "simpleError"))
    stop(message = paste("CSV import failed!", "filDataFile", sep = "|"))
  
  result
}