#' @name itemsEvaluation
#' @aliases itemsEvaluation
#' @title Non-executable auxiliary function
#' @usage itemsEvaluation(item,rriskModel,run=FALSE,catEval=FALSE)
#' @param item ...
#' @param rriskModel ...
#' @param run a single logical value
#' @param catEval a single logical value
#' @keywords items
#' @export

itemsEvaluation <- function(item, rriskModel, run = FALSE) {
  cat("itemEvaluation\n")
  
  # get data for item evaluation
  dataForWith <- get.dataForWith(rriskModel = rriskModel,
                                 item_name  = item@name, 
                                 run        = run)
  
  # set number of test evaluation to rriskModel@settings@Ntest
  if (run == FALSE)
    item@fullc <- gsub(x           = item@fullc, 
                       pattern     = "rriskModel@settings@N", 
                       replacement = "rriskModel@settings@Ntest")
  
  # versuche fullc-expression zu evaluieren
  if (length(dataForWith) > 0)
    try.result <- try(expr   = with(data = dataForWith, 
                                    eval(parse(text = item@fullc))),
                      silent = TRUE)
  else
    try.result <- try(expr   = eval(parse(text = item@fullc)), 
                      silent = TRUE)
  
  try.result
} 