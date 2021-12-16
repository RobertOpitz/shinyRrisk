#' @name get.dataForWith
#' @aliases get.dataForWith
#' @title Non-executable auxiliary function
#' @usage get.dataForWith(item,rriskModel,fullc=TRUE,run=FALSE,run2d=FALSE)
#' @param item ...
#' @param rriskModel  ...
#' @param fullc  ...
#' @param run  ...
#' @param run2d  ...
#' @keywords items
#' @export

get.dataForWith <- function(rriskModel,
                            item_name = "", # the item is only needed for item@name 
                            fullc = TRUE, 
                            run   = FALSE, 
                            run2d = FALSE) {
  cat("get.dataForWith\n")
  
  print(str(item_name))
  
  # define some help variables
  items        <- rriskModel@items@items
  # names of strata of all available strata-items
  strata.names <- list()
  # final output object
  dataForWith  <- list() 
  
  cat("get.dataForWith -> before loop\n")
  
  #---BEGIN loop: evaluate every item-------------------------------------------
  for (i in seq_along(items)) {
    if (is.null(items[[i]])) next # if item is empty, then nothing is to do 
    print(paste0("items[[i]]@name =", items[[i]]@name, "; item_name =", item_name))
    if (items[[i]]@name == item_name) break # only go through items loop till you go to the item itself
    
    #cat("get.dataForWith loop, i =", i, "\n")
    
    # set number of test evaluation to rriskModel@settings@Ntest
    if (run == FALSE) {
      items[[i]]@fullc  <- gsub(x           = items[[i]]@fullc,
                                pattern     = "rriskModel@settings@N",
                                replacement = "rriskModel@settings@Ntest")
      items[[i]]@relaxc <- gsub(x           = items[[i]]@relaxc,
                                pattern     = "rriskModel@settings@N",
                                replacement = "rriskModel@settings@Ntest")
    }
    
    #---BEGIN if: itemcode------------------------------------------------------
    if (items[[i]]@typecode == "stra") {
      cat("get.DataForWith -> stra\n")
      strata.names <- c(strata.names,
                        setNames(list(as.character(items[[i]]@data$stratum$stid)),
                                 items[[i]]@name))
      dataForWith <- c(dataForWith,
                       setNames(list(items[[i]]@data),
                                items[[i]]@name))

    } else if (items[[i]]@typecode == "data") { #---if: itemcode----------------
      cat("get.DataForWith -> fdoc\n")
      # sammele alle Datensätze
      dataForWith <- c(dataForWith,
                       setNames(list(items[[i]]@data),
                                items[[i]]@name))

    } else if (items[[i]]@typecode == "fdoc") { #---if: itemcode----------------
      
      cat("get.DataForWith -> fdoc; items[[", i, "]]@data =", items[[i]]@data, "\n")
      # sammele alle benutzerdefinierte Funktionen
      #assign(items[[i]]@name, items[[i]]@data, envir = .GlobalEnv) #?
      items[[i]]@name <- items[[i]]@data

    } else if (items[[i]]@typecode != "bdjp" & 
               items[[i]]@typecode != "stra") { #---if: itemcode----------------
      
      cat("get.DataForWith -> not bdjp & not stra\n")
      if (fullc == TRUE)
        textForWith <- items[[i]]@fullc
      else
        textForWith <- items[[i]]@relaxc
      
      cat("fullc =", fullc, "\n")
      print(str(dataForWith))
      print(textForWith)
      
      # TRY TO EVALUATE ITEM COMMAND
      tryResult <- try(with(data = dataForWith, 
                            eval(parse(text = textForWith))), 
                       silent = TRUE)
      print(str(tryResult))
      
      #---BEGIN if: handle try result-------------------------------------------
      if (inherits(tryResult, "try-error") | is.null(tryResult)) { 
        # evaluation of item command was not successful
        warning(paste("\nEvaluating", items[[i]]@name, "item...ERROR\n", 
                      "Item", items[[i]]@name, "could not be evaluated or", 
                      "has been evaluated to NULL!\n"),
                immediate. = TRUE)
        next # nothing else to do, go to next item
      } else { #---handle try result if-----------------------------------------
        # evaluation of item command was successful
        #---BEGIN if: handle specific situation---------------------------------
        if (items[[i]]@stratum != "" & items[[i]]@typecode != "mxrv"){
          
          stratum                   <- items[[i]]@stratum # stratum is not empty
          corresponding.strata.item <- which(names(strata.names) == stratum)
          
          #---BEGIN if: handle corresponding strata item------------------------
          if (length(corresponding.strata.item) > 0){
            
            if (items[[i]]@typecode == "numv")
              names(tryResult)    <- strata.names[[corresponding.strata.item]]
            else {
              tryResult           <- data.frame(tryResult)
              colnames(tryResult) <- strata.names[[corresponding.strata.item]]
            }
            
          } else { #---if: handle corresponding strata item---------------------
            warning(paste("There is no strata item that corresponds to the", 
                          " stratified item'", items[[i]]@name, "'!"),
                    immediate. = TRUE)
          } 
          #---END if: handle corresponding strata item--------------------------
        } 
        #---END if: handle specific situation-----------------------------------
        
        dataForWith <- c(dataForWith,
                         setNames(list(tryResult),
                                  items[[i]]@name))
        
        if (run2d == TRUE) { # muss das hier stattfinden? Kann das nicht die run2d Funktion machen?
          if (items[[i]]@typecode == "mcrv" & 
              (items[[i]]@rolecode == "u" | items[[i]]@rolecode == "uv")) {
            last <- length(dataForWith)
            dataForWith[[last]] <- as.vector(dataForWith[[last]])
          }
        }
      }
      #---END if: handle try result---------------------------------------------
    }
    #---END if: itemcode--------------------------------------------------------
  }
  #---END loop: evaluate every item---------------------------------------------
  dataForWith
}