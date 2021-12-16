# definition of "modelPartsClass"
# methods: "show"
#' S4 class for representing descriptions of all the parts of a \code{rrisk} model
#'
#' @name modelPartsClass-class
#' @aliases modelPartsClass
#' @docType class
#' @title S4 class for representing 'rrisk' model parts
#' @slot parts
#' @rdname modelPartsClass-class
#' @exportClass modelPartsClass
#' @examples
#' \donttest{new("modelPartsClass")}

setClass(Class          = "modelPartsClass",
         representation = representation(parts = "list"),
         prototype      = prototype(parts = list()))

#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelPartsClass}}
#'
#' @rdname show-methods
#' @aliases show,modelPartsClass-method
#' @docType methods
#' @title Show method for 'modelPartsClass'
#' @param object a \code{modelPartsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("modelPartsClass"))}

setMethod(f          = "show",
          signature  = signature(object = "modelPartsClass"),
          definition = function(object) {
            cat("*************************************************************************\n")
            cat("Model parts\n")
            cat("*************************************************************************\n")
            if (length(object@parts) == 0)
              cat("Model parts list is empty\n\n")
            for (parts in object@parts) { 
              show(parts)
              cat("\n")
            }
          }
)
