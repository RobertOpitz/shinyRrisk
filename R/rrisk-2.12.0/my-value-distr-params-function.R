#' @name myvalue.distrparams
#' @aliases myvalue.distrparams
#' @title Non-executable auxiliary function
#' @usage myvalue.distrparams(item,rriskModel,
#'        title="Tryme",menuLevel=1,
#'        minmax,asInteger=FALSE,batch=1)
#' @param item ...
#' @param rriskModel ...
#' @param title ...
#' @param menuLevel ...
#' @param minmax ...
#' @param asInteger boolean value giving whether entered value should be an integer, \code{asInteger=c(TRUE,FALSE)}
#' @param batch ...
#' @keywords items
#' @export

myvalue.distrparams <- function(item, rriskModel,
                                menuLevel = 1, minmax, asInteger = FALSE, 
                                batch = 1, valuesIn, value) {
  cat("myvalue.distrparams\n")
  
  text_to_parse <- valuesIn[[value]]
  
  # get data to evaluate current item
  dataForWith <- get.dataForWith(item, rriskModel, 
                                 run     = FALSE, 
                                 catEval = FALSE)
  
  # versuche text_to_parse-expression zu evaluieren
  if (length(dataForWith) > 0)
    values <- try(expr   = with(data = dataForWith, 
                                eval(parse(text = text_to_parse))), 
                  silent = TRUE)
  else
    values <- try(expr   = eval(parse(text = text_to_parse)), 
                  silent = TRUE)
  
  if (inherits(values, "try-error"))
    stop(message = paste("The expression cannot be evaluated because of unknown", 
                         "variable, data oder function!; value =", value), 
         call. = TRUE)
    
  # values muss im vordefinierten range liegen
  if (!missing(minmax)) {
    if (all(!is.na(minmax)) & length(values) == 1) {
      if (values < minmax[1] | values > minmax[2]) {
        stop(message = paste("Your input is out of range!", 
                             "c(", minmax[1], ",", minmax[2], ")",
                             "; value =", value), 
             call. = TRUE)
      }
    }
  }
    
  # muss values eine ganze zahl sein?
  if (asInteger == TRUE) {
    if (abs(values - round(values)) != 0)
      stop(message = paste("Your input is not integer!; value =", value), # ? is.integer ?
           call. = TRUE)
  }
    
  # und die Länge von values muss auch stimmen # ? implications if not?
  if (length(values) != batch)
    cat(lvlTab, "Entry does not evaluate to required length of ", batch, "\n")
    
  list(output = text_to_parse, 
       values = values, 
       error  = NA)
}