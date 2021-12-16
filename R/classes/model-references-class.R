# definition of "modelReferencesClass"
# methods: "show"
#' S4 class for representing all the references of a \code{rrisk} model
#'
#' @name modelReferencesClass-class
#' @aliases modelReferencesClass
#' @docType class
#' @title S4 class for representing 'rrisk' modeo references
#' @slot references
#' @rdname modelReferencesClass-class
#' @exportClass modelReferencesClass
#' @examples
#'  \donttest{new("modelReferencesClass")}

setClass(Class          = "modelReferencesClass",
         representation = representation(references = "character"),
         prototype      = prototype(references = ""))

#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelReferencesClass}}
#'
#' @rdname show-methods
#' @aliases show,modelReferencesClass-method
#' @docType methods
#' @title Show method for 'modelReferencesClass'
#' @param object a \code{modelReferencesClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("modelReferencesClass"))}

setMethod(f          = "show",
          signature  = signature(object = "modelReferencesClass"),
          definition = function(object) {
            cat("*************************************************************************\n")
            cat("References\n")
            cat("*************************************************************************\n")
            if (length(object@references) == 0) {
              cat("empty\n\n")
              return()
            }
            output <- sapply(object@references,
                             function(ref) {
                               if (ref != "") 
                                 ref <- paste0(putLineBreaks(ref), "\n")
                               else 
                                 ref <- "\n"
                               ref
                             })
            cat(output,"\n")
          }
)