# definition of "modelNameClass"
# methods: "show"
#' A S4 class for representing the names of \code{rrisk} models
#'
#' @name modelNameClass-class
#' @aliases modelNameClass
#' @docType class                                                                                              
#' @title S4 class for representing the names of 'rrisk' models
#' @slot name
#' @rdname modelNameClass-class
#' @exportClass modelNameClass
#' @examples
#' \donttest{new("modelNameClass")}

setClass(Class          = "modelNameClass",
         representation = representation(name = "character"),
         prototype      = prototype(name = "new rrisk model"))

#' Show method for \code{\linkS4class{modelNameClass}}
#'
#' @rdname show-methods
#' @aliases show                                         
#' @docType methods
#' @title Show method for 'modelNameClass'
#' @param object a \code{modelNameClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("modelNameClass"))}

setMethod(f          = "show",
          signature  = signature(object = "modelNameClass"),
          definition = function(object) { 
            cat("=========================================================================\n")
            cat("Model name\n")
            cat("=========================================================================\n")
            cat(object@name,"\n\n")
          })