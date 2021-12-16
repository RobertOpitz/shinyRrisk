#' S4 class for representing all the graphs used in \code{rrisk} models
#'
#' @name modelGraphsClass-class
#' @aliases modelGraphsClass
#' @docType class
#' @title S4 class for representing all 'rrisk' model graphs objects
#' @slot graphs
#' @rdname modelGraphsClass-class
#' @exportClass modelGraphsClass
#' @examples
#'  \donttest{new("modelGraphsClass")}

setClass(Class          = "modelGraphsClass",
         representation = representation(graphs = "list"),
         prototype      = prototype(graphs = list()))

#-------------------------------------------------------------------------------
#' Show method for \code{\linkS4class{modelGraphsClass}}
#'
#' @rdname show-methods
#' @aliases show, modelGraphsClass-method
#' @docType methods
#' @title Show method for 'modelGraphsClass'
#' @param object a \code{modelGraphsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("modelGraphsClass"))}

setMethod(f          = "show",
          signature  = signature(object="modelGraphsClass"),
          definition = function(object) {
            cat("*************************************************************************\n")
            cat("Model graphs\n")
            cat("*************************************************************************\n")
            if (length(object@graphs) == 0)
              cat("Model graphs list is empty\n\n")
            for (object_graph in object@graphs){ 
              show(object_graphs)
              cat("\n")
            }
          })