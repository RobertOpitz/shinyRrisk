#' @name getRuleSet
#' @title getRuleSet
#' @description Gets rule function to be applied.
#' @param rule Rule name.
#' @return Rule as function.
#' @export
getRuleSet <- function(rule) {
  cat("getRuleSet\n")
  
  switch(rule,
         "required" = function(value, compare = NA) {
           trimmed <- trimws(value)
           trimmed != "" && !is.null(trimmed) && !is.na(trimmed)},

         "unique" = function(value, compare) !(value %in% compare),

         "startsWithLetter" = function(value, compare = NA) 
           substring(value, 1, 1) %in% letters ||
           tolower(substring(value, 1, 1)) %in% letters,

         "isAlphanumeric" = function(value, compare = NA) 
           grepl("^[A-Za-z1-9]+$", value),

         "isNumeric" = function(value, compare = NA) 
           grepl("^[[:digit:]]", value),

         "isInteger" = function(value, compare = NA)
            grepl("^[[:digit:]]", value) &&
            as.numeric(value) %% 1 == 0, # ? warum nicht base::is.integer ?

         "isValidEmail" = function(value, compare = NA)
            grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", 
                  as.character(value), 
                  ignore.case = TRUE),

         "smallerThan" = function(value, compare) value < as.numeric(compare),

         "greaterThan" = function(value, compare) value > as.numeric(compare),

         "limit" = function(value, compare)
            nchar(as.character(value), keepNA = FALSE) <= as.numeric(compare),
         
         "isValidCode" = function(value, compare = NA) {
           result <- tryCatch(parse(text = value), 
                              error = function(e) e)
           if (is.expression(result))
             TRUE
           else
             FALSE
         }
  )

}
