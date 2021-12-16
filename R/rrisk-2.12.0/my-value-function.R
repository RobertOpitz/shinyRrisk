#' @description Function that evaluates and saves the input value for item objects from the
#' rriskModel class
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name myvalue
#' @aliases myvalue
#' @title Non-executable auxiliary function
#' @usage myvalue(item,rriskModel,title="Tryme",menuLevel=1,batch=1,run=FALSE)
#' @param item ...
#' @param rriskModel ...
#' @param title ... 
#' @param menuLevel ...
#' @param batch ...
#' @param run boolean value defining whether this function nwill be calls during 1d or 2d simulation, \code{run=c(TRUE,FALSE)}
#' @keywords items
#' @export

myvalue <- function(item, rriskModel,
                    menuLevel = 1, batch = 1, 
                    run = FALSE, value) {
  cat("myvalue\n")
  
  # get data to evaluate current item
  dataForWith <- get.dataForWith(rriskModel = rriskModel,
                                 item_name  = item@name,
                                 run        = run) # nur 1d funktionalität, kein relax mode
  
  print(str(dataForWith))
  print(value)
  
  result <- list(output = NA, values = NA)
  
  # versuche value-expression zu evaluieren
  if (length(dataForWith) > 0)
    result$values <- try(with(data = dataForWith, 
                              eval(parse(text = value))), 
                         silent = TRUE)
  else
    result$values <- try(eval(parse(text = value)), 
                         silent = TRUE)
  
  str(result$values)

  #---BEGIN if: what to do after eval-------------------------------------------
  if (inherits(result$values, "try-error"))
    warning(paste("The expression cannot be evaluated because of", 
                  "unknown variable, data oder function!"),
            immediate. = TRUE)
  else { #---if: what to do after eval------------------------------------------
    
    # und die Länge von values muss auch stimmen
    if (item@typecode == "numv") {
      if (length(result$values) != batch)
        warning(paste("Entry does not evaluate to required length of",
                   "batch:", batch),
                immediate. = TRUE)
    } else if (item@typecode == "fnrv") {
      cat("myvalue -> successful evaluation -> fnrv")
      if (nrow(result$values) != rriskModel@settings@N | 
          ncol(result$values) != batch)
        warning(paste("Entry does not evaluate to required number of",
                      "rows:", requiredRows, "and batch:", batch),
                immediate. = TRUE)
    }
    
    result$output <- value
  }
  #---END if: what to do after eval---------------------------------------------
  
  result
}
