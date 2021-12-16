# definition of "modelItemClass"
# methods: "show", "toList"
#' S4 class for representing all the Items of \code{rrisk} models
#'
#' @name modelItemsClass-class
#' @aliases modelItemsClass
#' @docType class
#' @title S4 class for representing all 'rrisk' model items
#' @slot items
#' @rdname modelItemsClass-class
#' @exportClass modelItemsClass
#' @examples
#'  \donttest{new("modelItemsClass")}
#'  
setClass(Class          = "modelItemsClass",
         representation = representation(items = "list"),
         prototype      = prototype(items = list()))

#' Show method for \code{\linkS4class{modelItemsClass}}
#'
#' @rdname show-methods
#' @aliases show, modelItemsClass-method
#' @docType methods
#' @title Show method for 'modelItemsClass'
#' @param object a \code{modelItemsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("modelItemsClass"))}

setMethod(f          = "show",
          signature  = signature(object = "modelItemsClass"),
          definition = function(object) {
            cat("*************************************************************************\n")
            cat("Model items\n")
            cat("*************************************************************************\n")
            if (length(object@items) == 0)
              cat("Model items list is empty\n\n")
            n <- length(object@items)
            for(i in seq_len(n)) { 
              show(object@items[[i]])
              if (i != n)
                cat("-----------------------------------------------------------------------------\n")
            }
            cat("\n")
          }
)

#' toList method for \code{\linkS4class{modelItemsClass}}
#'
#' @rdname toList-methods
#' @aliases toList, modelItemsClass-method
#' @docType methods
#' @title toList method for 'modelItemsClass'
#' @param object a \code{modelItemsClass} object
#' @exportMethod toList
##' @importFrom methods toList
#' @examples
#'  \donttest{toList(new("modelItemsClass"))}

setMethod(f          = "toList", 
          signature  = signature(object = "modelItemsClass"), 
          definition = function(object) lapply(object@items, toList))
