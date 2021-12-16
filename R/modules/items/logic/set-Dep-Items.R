#' @name setDepItems
#' @aliases setDepItems
#' @title Non-executable auxiliary function
#' @usage setDepItems(item,items)
#' @param item ...
#' @param items ... 
#' @keywords manip
#' @export

setDepItems <- function(item, items) {
  cat("setDepItems\n")
  
  # internal helper function
  get_allTerms2 <- function(split_string) {
    #cat("get_allTerms2\n")
    all_Terms2 <- NULL
    helpvar <- as.list(strsplit(item@definition,
                                split = split_string)[[1]])
    for (this_helpvar in helpvar) {
      temp <- try(expr   = all.vars(parse(text = this_helpvar)),
                  silent = TRUE)
      if (!inherits(temp, "try-error")) # no error occured
        all_Terms2 <- c(all_Terms2, temp)
    }
    all_Terms2
  }
  
  allTerms1 <- all.names(parse(text = item@fullc))
  
  if (item@typecode %in% c("bsrv","mcrv","bdjp"))
    allTerms2 <- get_allTerms2("; fitted to")
  else if(item@typecode == "mxrv")
    allTerms2 <- get_allTerms2(" based on: ")
  else
    allTerms2 <- NULL
  
  allTerms3 <- item@stratum
  
  allTerms <- c(allTerms1, allTerms2, allTerms3)
  allTerms <- setdiff(allTerms, "")
  
  #cat("\n-------------item------------------\n")
  #print(item)
  #cat("\n-----------------------------------\n\n")
  
  #cat("allTerms\n")
  #print(allTerms)
  
  # what happens here? So strange.
  for (i in seq_along(items)) {
    #cat("items[[", i, "]]@name = ",    items[[i]]@name,"\n")
    if (items[[i]]@name %in% allTerms) {
      #cat("item@name = ",                item@name, "\n")
      #cat("Vorher: items[[", i, "]]@depitem = ", items[[i]]@depitem, "\n")
      items[[i]]@depitem <- paste(items[[i]]@depitem, item@name, collapse = " ")
      #cat("Nachher: items[[", i, "]]@depitem = ", items[[i]]@depitem, "\n")
      depitem            <- strsplit(x = items[[i]]@depitem, split = " ")
      #cat("depitem = ")
      #print(depitem)
      #cat("\n")
      if (length(depitem) > 0) { #? so weird
        depitem            <- unique(depitem[[1]])
        #cat("unique depitem = ", depitem, "\n")
        items[[i]]@depitem <- paste(depitem, collapse = " ") # there cn be only ONE OR MORE depitem?
        #cat("Processed: items[[", i, "]]@depitem = ", items[[i]]@depitem, "\n\n")
      } # what is going on here, and why?
    }
  }
  
  items
}