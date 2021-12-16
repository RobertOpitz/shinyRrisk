# definition of "modelUncertaintiesClass"
# methods: "show", "toList"
#' S4 class for representing descriptions of all the uncertainties of a \code{rrisk} 
#' model
#'
#' @name modelUncertaintiesClass-class
#' @aliases modelUncertaintiesClass
#' @docType class
#' @title S4 class for representing 'rrisk' model uncertainties
#' @slot note
#' @slot uncertainties
#' @rdname modelUncertaintiesClass-class
#' @exportClass modelUncertaintiesClass
#' @examples
#' \donttest{new(Class="modelUncertaintiesClass")}

setClass(Class          = "modelUncertaintiesClass",
         representation = representation(note          = "character",
                                         uncertainties = "list"),
         prototype      = prototype(note          = "", # ? what is note
                                    uncertainties = list()))

#' Show method for \code{\linkS4class{modelUncertaintiesClass}}
#'
#' @rdname show-methods
#' @aliases show,modelUncertaintiesClass-method
#' @docType methods
#' @title Show method for 'modelUncertaintiesClass'
#' @param object a \code{modelUncertaintiesClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new(Class="modelUncertaintiesClass"))}

setMethod(f          = "show",
          signature  = signature(object = "modelUncertaintiesClass"),
          definition = function(object) { 
            cat("*************************************************************************\n")
            cat("Model uncertainties\n")
            cat("*************************************************************************\n")
            if (object@note == "" & length(object@uncertainties) == 0)
              cat("Model uncertainties list is empty \n\n")
            else {
              cat("Note: ", putLineBreaks(object@note), "\n\n")
              if (length(object@uncertainties) == 0)
                cat("Uncertainties: \n\n")
              for (uncertainties in object@uncertainties) { 
                show(uncertainties)
                cat("\n")
              }
            }
          })

#' toList method for \code{\linkS4class{modelUncertaintiesClass}}
#'
#' @rdname toList-methods
#' @aliases toList, modelUncertaintiesClass-method
#' @docType methods
#' @title toList method for 'modelUncertaintiesClass'
#' @param object a \code{modelUncertaintiesClass} object
#' @exportMethod toList
##' @importFrom methods toList
#' @examples
#' \donttest{toList(new(Class="modelUncertaintiesClass"))}

setMethod(f          = "toList", 
          signature  = signature(object = "modelUncertaintiesClass"), 
          definition = function(object) lapply(object@uncertainties, toList))

setMethod(f          = "toDataFrame", 
          signature  = signature(object = "modelUncertaintiesClass"), 
          definition = function(object) {
            cat("call to method toDataFrame\n")
            # create the data frame
            df <- NULL
            for (uncert in object@uncertainties) {
              # get new row
              df_row <- toList(uncert)
              # transform the list object into a data.frame object
              df_row <- as.data.frame(df_row)
              # bind new row to data frame
              df <- rbind(df, df_row)
            }
            df
          })
