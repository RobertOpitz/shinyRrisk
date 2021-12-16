#' @name getErrorMessage
#' @title getErrorMessage
#' @description Gets error message of given error name.
#' @param error Error name.
#' @return Error message.
#' @export
getErrorMessage <- function(error) {
  cat("getErrorMessage\n")
  switch(error,
         "required"          = "Provide a value",
         "unique"            = "Provided value must be unique",
         "startsWithLetter"  = "Provided value must start with a letter",
         "isAlphanumeric"    = "Provided value must be alphanumeric",
         "isNumeric"         = "Provided value has to be numeric",
         "isInteger"         = "Provided value has to be an integer",
         "isValidEmail"      = "Provide a valid email address",
         "smallerThan"       = "Provided value needs to be smaller",
         "greaterThan"       = "Provided value needs to be greater",
         "limit"             = "Provided value must have less characters",
         "isValidCode"       = "Error in your provided code")
}
